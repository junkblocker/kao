;;; sam-preview.el --- Isolated text-only Sam recipe previews -*- lexical-binding: t; -*-

;; 2026-10-21: Use the real parser/evaluator on temporary buffers. Never
;; execute shell/undo recipes to preview them, mutate user buffers, or run
;; normal editing hooks in preview. Apply reruns only a validated snapshot;
;; native per-buffer undo and partial-failure semantics remain.
(require 'sam)
(require 'cl-lib)

(defun sam-preview--nodes (command)
  "Flatten parsed COMMAND nodes without changing regex history."
  (let ((sam--last-regex sam--last-regex) nodes)
    (cl-labels ((walk (node)
                  (when node
                    (let ((op (cadr node)))
                      (push node nodes)
                      (pcase op
                        ('block (mapc #'walk (nth 2 node)))
                        ((or 'x 'y 'g 'v 'X 'Y) (walk (nth 3 node))))))))
      (mapc #'walk (sam-parse command)))
    nodes))

(defun sam-command-operations (command)
  "Return operations used by Sam COMMAND without changing regex history."
  (delete-dups (mapcar #'cadr (sam-preview--nodes command))))

(defun sam-preview--candidates (nodes)
  "Find a conservative universe for buffer loops in parsed NODES.
Check both case-fold settings because nested loops can run in other buffers.
Concrete file/name patterns should not copy unrelated large editor buffers."
  (let (matches)
    (dolist (node nodes)
      (when (memq (cadr node) '(X Y))
        (dolist (fold '(nil t))
          (let ((case-fold-search fold))
            (setq matches (append (sam--matching-buffers (nth 2 node) (eq (cadr node) 'X)) matches))))))
    (cl-remove-if-not (lambda (buffer) (memq buffer matches)) (buffer-list))))

(defun sam-preview--mark ()
  (when sam--mark
    (cons (marker-position (car sam--mark)) (marker-position (cdr sam--mark)))))

(defun sam-preview--table-data (table)
  "Capture comparable character ranges, including inherited TABLE entries.
Emacs compares character tables by identity, even after copying them."
  (let (ranges)
    (map-char-table (lambda (range value) (push (cons range value) ranges)) table)
    ranges))

(defun sam-preview--snapshot (buffer)
  (with-current-buffer buffer
    (list :buffer buffer :tick (buffer-chars-modified-tick)
          :name (buffer-name) :file buffer-file-name :mode major-mode
          :min (point-min) :max (point-max) :case-fold case-fold-search
          :syntax (copy-syntax-table (syntax-table))
          :case-table (copy-case-table (current-case-table))
          :syntax-data (sam-preview--table-data (syntax-table))
          :case-data (sam-preview--table-data (current-case-table))
          :mark (sam-preview--mark)
          :before (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun sam-preview (command &optional buffer)
  "Return a text-only preview plan for COMMAND on BUFFER, without editing it.
Shell operations and undo are rejected, including inside nested blocks.
Buffer loops see isolated copies with original names/file labels. Copies
have no normal editing hooks; hook-induced changes during actual apply are
not predicted. Regex history, saved marks and output buffers stay untouched.
Use `sam-apply-preview' to validate and execute the reviewed plan."
  (let* ((buffer (or buffer (current-buffer)))
         (nodes (sam-preview--nodes command))
         (operations (mapcar #'cadr nodes))
         (initial (with-current-buffer buffer (sam--current-dots)))
         (regex sam--last-regex)
         (originals (sam-preview--candidates nodes))
         (labels (make-hash-table :test 'eq))
         (owners (make-hash-table :test 'eq))
         (snapshots (make-hash-table :test 'eq))
         (inhibit-modification-hooks t)
         copies touched loops output)
    (when (cl-intersection operations '(| < > ! u))
      (sam--fail "preview accepts text commands only; shell and undo cannot be simulated"))
    (cl-pushnew buffer originals)
    (unwind-protect
        (progn
          (dolist (original originals)
            (let* ((snapshot (sam-preview--snapshot original))
                   (copy (generate-new-buffer " *sam preview copy*")))
              (push copy copies)
              (puthash copy original owners)
              (puthash copy snapshot snapshots)
              (puthash copy (cons (plist-get snapshot :name)
                                  (or (plist-get snapshot :file) (plist-get snapshot :name))) labels)
              (with-current-buffer copy
                (insert (plist-get snapshot :before))
                (set-syntax-table (copy-syntax-table (plist-get snapshot :syntax)))
                (set-case-table (copy-case-table (plist-get snapshot :case-table)))
                (setq-local case-fold-search (plist-get snapshot :case-fold))
                (when (plist-get snapshot :mark) (sam--save-mark (plist-get snapshot :mark)))
                (narrow-to-region (plist-get snapshot :min) (plist-get snapshot :max)))))
          (let ((sam--last-regex regex)
                (sam--buffer-list (reverse copies))
                (sam--buffer-labels labels)
                (sam--inhibit-output t)
                (inhibit-modification-hooks t)
                (sam--run-observer (lambda (copy) (cl-pushnew copy touched)))
                (sam--loop-observer
                 (lambda (re positive matches)
                   (push (list re positive case-fold-search
                               (mapcar (lambda (copy) (gethash copy owners)) matches)) loops))))
            (setq output (sam-execute command (cl-find buffer copies :key (lambda (copy) (gethash copy owners))) initial)))
          (list :command command :target buffer :dots initial :regex regex :output output
                :loops loops
                :snapshots
                (mapcar (lambda (copy)
                          (let ((snapshot (gethash copy snapshots)))
                            (plist-put snapshot :after
                                       (with-current-buffer copy
                                         (save-restriction (widen) (buffer-string))))))
                        (reverse touched))))
      (dolist (copy copies)
        (when (buffer-live-p copy)
          (with-current-buffer copy (set-buffer-modified-p nil))
          (let (kill-buffer-hook kill-buffer-query-functions)
            (kill-buffer copy)))))))

(defun sam-preview--unchanged-p (snapshot)
  (let ((buffer (plist-get snapshot :buffer)))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (= (buffer-chars-modified-tick) (plist-get snapshot :tick))
                (equal (buffer-name) (plist-get snapshot :name))
                (equal buffer-file-name (plist-get snapshot :file))
                (eq major-mode (plist-get snapshot :mode))
                (= (point-min) (plist-get snapshot :min))
                (= (point-max) (plist-get snapshot :max))
                (eq case-fold-search (plist-get snapshot :case-fold))
                (equal (sam-preview--table-data (syntax-table)) (plist-get snapshot :syntax-data))
                (equal (sam-preview--table-data (current-case-table)) (plist-get snapshot :case-data))
                (equal (sam-preview--mark) (plist-get snapshot :mark)))))))

;;;###autoload
(defun sam-apply-preview (plan)
  "Validate reviewed PLAN, then run its recipe on the captured starting dots.
Reject changed buffers, target selection, scope, regex history or buffer-loop
membership before any edit. Read-only changed buffers are rejected too.
Normal editing hooks run during apply and can still fail or alter results.
Earlier completed commands/buffers survive errors: undo in each buffer.
Kao retains its native command-boundary history unit for a whole program."
  (let ((target (plist-get plan :target))
        (snapshots (plist-get plan :snapshots)))
    (unless (and (buffer-live-p target) (equal sam--last-regex (plist-get plan :regex))
                 (cl-every #'sam-preview--unchanged-p snapshots)
                 (with-current-buffer target (equal (sam--current-dots) (plist-get plan :dots))))
      (sam--fail "preview is stale; review a fresh preview before applying"))
    (dolist (loop (plist-get plan :loops))
      (let ((case-fold-search (nth 2 loop)))
        (unless (equal (sam--matching-buffers (nth 0 loop) (nth 1 loop)) (nth 3 loop))
          (sam--fail "preview buffer targets changed; review again"))))
    (dolist (snapshot snapshots)
      (when (and (not (equal (plist-get snapshot :before) (plist-get snapshot :after)))
                 (buffer-local-value 'buffer-read-only (plist-get snapshot :buffer)))
        (sam--fail "read-only preview target: %s" (plist-get snapshot :name))))
    (sam-execute (plist-get plan :command) target (plist-get plan :dots))))

(provide 'sam-preview)
;;; sam-preview.el ends here
