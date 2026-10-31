;;; sam.el --- sam structural regular expression commands -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; 2026-10-31: Licensed by the user for this combined Kao/Sam fork.

;;; Commentary:

;; An implementation of the sam(1) command language for Emacs buffers,
;; with kao selections as sam's "dot".
;; 2026-10-20: At top level, an explicit address independent of dot runs
;; once; relative/unaddressed commands run per selection. Nested loops
;; retain their deliberate repetition. This keeps saved recipes predictable
;; without changing Kao's selection or repeat semantics.
;; 2026-10-31: Sam and Kao share this package. Keep their public selection
;; interface: Sam uses half-open ranges; Kao uses inclusive selections.
;;
;; Addresses: #n  n  /re/  ?re?  $  .  '  a+b  a-b  a,b  a;b
;; Commands:  a/t/ i/t/ c/t/ d  s[n]/re/t/[g]  p  =  k
;;            x/re/ cmd  y/re/ cmd  g/re/ cmd  v/re/ cmd  { cmd NL cmd }
;;            X/re/ cmd  Y/re/ cmd  m addr  t addr  |cmd  <cmd  >cmd  !cmd  u[n]
;;
;; Regexes are Emacs regexes (the same syntax as kao's prompts).
;;
;; Like sam, one command line is evaluated against the unchanged buffer:
;; every edit is recorded as (BEG END TEXT) in original coordinates, the
;; edits are checked for overlap ("changes not in sequence"), then applied
;; back to front as one undo step. Match/change ranges use integers, so
;; loops leave no markers behind. 2026-10-21: Only the explicit saved mark
;; owns two markers, keeping its text attached across ordinary buffer edits.
;;
;; Like vis and Kakoune, a loop with no command (`,x/re/') selects its
;; matches: the ranges left by the last command become kao selections
;; (or the region when kao is off).
;;
;; `sam' prompts for a command.  `sam-scratch' opens an acme-style buffer
;; where RET runs the command line under point against the last buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function kao-get-selections "kao-state")
(declare-function kao-set-selections "kao-state")
(declare-function kao-sel-beg "kao-selection")
(declare-function kao-sel-end "kao-selection")
(declare-function kao-sel-make "kao-selection")
(declare-function kao-normal-state-p "kao-state")
(declare-function kao-edit-keeping-selections "kao-edit")

(defgroup sam nil
  "sam structural regular expression commands."
  :group 'editing)

(defvar sam-history nil
  "Minibuffer history for `sam'.")

(defvar-local sam--mark nil
  "The saved Sam range: a pair of buffer-owned markers, or nil.
`k' sets it; the apostrophe address reads an integer snapshot. Insertion
at either boundary joins the range; deleting it collapses both endpoints.
This is separate from the Emacs mark and Kao selections.")

;;;###autoload
(defun sam-clear-mark ()
  "Forget the current buffer's saved Sam range and release its markers.
`k' replaces the saved range; changing major mode also clears it.
Saving a file does not persist the range across Emacs sessions."
  (interactive)
  (when sam--mark
    (set-marker (car sam--mark) nil)
    (set-marker (cdr sam--mark) nil)
    (setq sam--mark nil)))

(defun sam--save-mark (range)
  "Save RANGE using exactly two markers in the current buffer."
  (sam-clear-mark)
  ;; 2026-10-21: Opposite insertion biases include replacement text and
  ;; avoid inverted endpoints when an empty/deleted range is inserted into.
  ;; Replacing k detaches the previous pair, never accumulating per-match
  ;; markers like the removed engine. Other evaluation ranges stay integers.
  (setq sam--mark (cons (copy-marker (car range))
                        (copy-marker (cdr range) t)))
  (add-hook 'change-major-mode-hook #'sam-clear-mark nil t))

(defvar sam--last-regex nil
  "The last regex used, reused by an empty /re/.")

(define-error 'sam-error "sam")

(defun sam--fail (fmt &rest args)
  (signal 'sam-error (list (apply #'format fmt args))))

;;;; Parser

(defvar sam--s nil "The command string being parsed.")
(defvar sam--i 0 "Parse position in `sam--s'.")

(defun sam--peek () (and (< sam--i (length sam--s)) (aref sam--s sam--i)))
(defun sam--next () (prog1 (sam--peek) (cl-incf sam--i)))

(defun sam--skip-ws ()
  (while (memq (sam--peek) '(?\s ?\t)) (cl-incf sam--i)))

(defun sam--number ()
  (when (and (sam--peek) (<= ?0 (sam--peek) ?9))
    (let ((start sam--i))
      (while (and (sam--peek) (<= ?0 (sam--peek) ?9)) (cl-incf sam--i))
      (string-to-number (substring sam--s start sam--i)))))

(defun sam--delimited (delim &optional text)
  "Read up to an unescaped DELIM.
TEXT non-nil: expand \\n and \\\\ (text arguments).  TEXT `regex': expand
\\n to a newline, as sam does, and keep every other escape for Emacs."
  (let ((out nil))
    (catch 'done
      (while t
        (let ((c (sam--next)))
          (cond
           ((null c) (throw 'done nil))
           ((eq c delim) (throw 'done nil))
           ((and (eq c ?\\) (sam--peek))
            (let ((n (sam--next)))
              (cond ((eq n delim) (push (string n) out))
                    ((and text (eq n ?n)) (push "\n" out))
                    ((and (eq text t) (eq n ?\\)) (push "\\" out))
                    (t (push (string ?\\ n) out)))))
           (t (push (string c) out))))))
    (apply #'concat (nreverse out))))

(defun sam--regex-arg (delim)
  (let ((re (sam--delimited delim 'regex)))
    (if (string-empty-p re)
        (or sam--last-regex (sam--fail "no previous regex"))
      (setq sam--last-regex re))))

(defun sam--simple-address ()
  (sam--skip-ws)
  (let ((c (sam--peek)))
    (cond
     ((null c) nil)
     ((eq c ?#) (sam--next) (list 'char (or (sam--number) 0)))
     ((<= ?0 c ?9) (list 'line (sam--number)))
     ((eq c ?/) (sam--next) (list 'fwd (sam--regex-arg ?/)))
     ((eq c ??) (sam--next) (list 'bwd (sam--regex-arg ??)))
     ((eq c ?$) (sam--next) '(end))
     ((eq c ?.) (sam--next) '(dot))
     ((eq c ?') (sam--next) '(mark)))))

(defun sam--address2 ()
  (let ((left (sam--simple-address)))
    (sam--skip-ws)
    (while (memq (sam--peek) '(?+ ?-))
      (let* ((op (if (eq (sam--next) ?+) 'plus 'minus))
             (right (or (sam--simple-address) '(line 1))))
        (setq left (list op (or left '(dot)) right))
        (sam--skip-ws)))
    left))

(defun sam--address ()
  (let ((a (sam--address2)))
    (sam--skip-ws)
    (if (memq (sam--peek) '(?, ?\;))
        (let ((op (if (eq (sam--next) ?,) 'comma 'semi))
              (b (sam--address)))
          (list op (or a '(line 0)) (or b '(end))))
      a)))

(defun sam--rest-of-line ()
  (let ((start sam--i))
    (while (and (sam--peek) (not (eq (sam--peek) ?\n))) (cl-incf sam--i))
    (string-trim (substring sam--s start sam--i))))

(defun sam--text-arg ()
  (sam--skip-ws)
  (let ((d (sam--next)))
    (unless (and d (not (memq d '(?\n ?\s))))
      (sam--fail "missing text delimiter"))
    (sam--delimited d t)))

(defun sam--command ()
  "Parse one command.  Return (ADDR OP . ARGS), or nil at end of input."
  (sam--skip-ws)
  (while (eq (sam--peek) ?\n) (sam--next) (sam--skip-ws))
  (let ((addr (sam--address)))
    (sam--skip-ws)
    (let ((c (sam--peek)))
      (cond
       ((or (null c) (eq c ?\n) (eq c ?}))
        (and addr (list addr 'select)))
       (t
        (sam--next)
        (pcase c
          ((or ?a ?i ?c) (list addr (intern (string c)) (sam--text-arg)))
          ((or ?d ?p ?= ?k) (list addr (intern (string c))))
          (?s (let* ((n (sam--number))
                     (d (progn (sam--skip-ws) (sam--next)))
                     (re (sam--regex-arg d))
                     (to (sam--delimited d t))
                     (g (when (eq (sam--peek) ?g) (sam--next) t)))
                (list addr 's re to (or n 1) g)))
          ((or ?x ?y ?g ?v ?X ?Y)
           (sam--skip-ws)
           (let* ((d (sam--peek))
                  (re (if (and d (not (memq d '(?\n ?{)))
                               (not (string-match-p "[[:alnum:]]" (string d))))
                          (progn (sam--next) (sam--regex-arg d))
                        (if (memq c '(?x ?y)) ".*\n"
                          (sam--fail "%c needs a regex" c))))
                  (sub (sam--command)))
             (list addr (intern (string c)) re sub)))
          ((or ?m ?t) (list addr (intern (string c))
                            (or (sam--address) (sam--fail "%c needs an address" c))))
          ((or ?| ?< ?> ?!) (list addr (intern (string c)) (sam--rest-of-line)))
          (?u (list addr 'u (or (sam--number) 1)))
          (?{ (let (cmds cmd)
                (while (progn (sam--skip-ws)
                              (while (eq (sam--peek) ?\n) (sam--next) (sam--skip-ws))
                              (not (memq (sam--peek) '(?} nil))))
                  (setq cmd (sam--command))
                  (if cmd (push cmd cmds) (sam--fail "bad command in {}")))
                (unless (eq (sam--next) ?}) (sam--fail "missing }"))
                (list addr 'block (nreverse cmds))))
          (_ (sam--fail "unknown command `%c'" c))))))))

(defun sam-parse (string)
  "Parse STRING into a list of top-level commands (one per line)."
  (let ((sam--s string) (sam--i 0) cmds)
    (while (< sam--i (length sam--s))
      (let ((cmd (sam--command)))
        (when cmd (push cmd cmds))
        (sam--skip-ws)
        (cond ((null (sam--peek)))
              ((eq (sam--peek) ?\n) (sam--next))
              (t (sam--fail "unexpected `%s'" (substring sam--s sam--i))))))
    (nreverse cmds)))

;;;; Addresses (evaluated in the current buffer, original coordinates)

(defun sam--line-range (n)
  (save-excursion
    (goto-char (point-min))
    (if (zerop n)
        (cons (point-min) (point-min))
      (unless (zerop (forward-line (1- n)))
        (sam--fail "address out of range"))
      (when (and (eobp) (> n 1) (not (bolp))) (sam--fail "address out of range"))
      (cons (point) (progn (forward-line 1) (point))))))

(defun sam--search (re from backward)
  "Find RE from FROM, wrapping around like sam.  Return (BEG . END)."
  (save-excursion
    (let ((case-fold-search nil))
      (goto-char from)
      (if (or (if backward (re-search-backward re nil t) (re-search-forward re nil t))
              (progn (goto-char (if backward (point-max) (point-min)))
                     (if backward (re-search-backward re nil t) (re-search-forward re nil t))))
          (cons (match-beginning 0) (match-end 0))
        (sam--fail "no match for %s" re)))))

(defun sam--lines-from (pos n backward)
  (save-excursion
    (goto-char pos)
    (if backward
        (forward-line (- n))
      (forward-line (if (and (bolp) (> n 0)) (1- n) n)))
    (cons (line-beginning-position) (min (point-max) (1+ (line-end-position))))))

(defun sam--resolve (addr dot)
  (pcase addr
    ('nil dot)
    (`(char ,n) (let ((p (+ (point-min) n)))
                  (when (> p (point-max)) (sam--fail "address out of range"))
                  (cons p p)))
    (`(line ,n) (sam--line-range n))
    (`(fwd ,re) (sam--search re (cdr dot) nil))
    (`(bwd ,re) (sam--search re (car dot) t))
    ('(end) (cons (point-max) (point-max)))
    ('(dot) dot)
    ('(mark)
     (unless sam--mark (sam--fail "no mark"))
     (let ((b (marker-position (car sam--mark)))
           (e (marker-position (cdr sam--mark))))
       (unless (and b e (<= (point-min) b e (point-max)))
         (sam--fail "mark outside accessible buffer"))
       ;; Snapshot before queued edits: parallel commands must not retain
       ;; live markers inside their original-coordinate edit/result records.
       (cons b e)))
    (`(,(and op (or 'plus 'minus)) ,l ,r)
     (let ((base (sam--resolve l dot)) (back (eq op 'minus)))
       (pcase r
         (`(line ,n) (sam--lines-from (if back (car base) (cdr base)) n back))
         (`(fwd ,re) (sam--search re (if back (car base) (cdr base)) back))
         (`(bwd ,re) (sam--search re (if back (car base) (cdr base)) (not back)))
         (_ (sam--resolve r base)))))
    (`(comma ,a ,b)
     (let ((ra (sam--resolve a dot)) (rb (sam--resolve b dot)))
       (when (< (cdr rb) (car ra)) (sam--fail "addresses out of order"))
       (cons (car ra) (cdr rb))))
    (`(semi ,a ,b)
     (let* ((ra (sam--resolve a dot)) (rb (sam--resolve b ra)))
       (when (< (cdr rb) (car ra)) (sam--fail "addresses out of order"))
       (cons (car ra) (cdr rb))))
    (_ (sam--fail "bad address %S" addr))))

;;;; Evaluation

(defvar sam--changes nil "Changes for the current buffer: (BEG END TEXT SEQ).")
(defvar sam--results nil "Ranges to select afterwards: (range B . E) or (change . C).")
(defvar sam--seq 0)
(defvar sam--output nil "Text printed by p = > !.")
(defvar sam--buffer-list nil "Private preview buffer universe, or nil for real buffers.")
(defvar sam--buffer-labels nil "Private preview labels: buffer -> (NAME . FILE-OR-NAME).")
(defvar sam--run-observer nil "Private preview callback receiving each evaluated buffer.")
(defvar sam--loop-observer nil "Private preview callback receiving REGEX POSITIVE MATCHES.")
(defvar sam--inhibit-output nil "Suppress output-buffer changes during isolated preview.")

(defun sam--matching-buffers (regex positive)
  "Find visible buffers matching REGEX when POSITIVE, or its complement.
The preview environment supplies isolated copies with original labels."
  (cl-remove-if-not
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (let* ((labels (and sam--buffer-labels (gethash buffer sam--buffer-labels)))
                 (name (or (car labels) (buffer-name buffer)))
                 (label (or (cdr labels) (buffer-file-name buffer) name)))
            (and (not (string-prefix-p " " name))
                 (eq positive (and (string-match-p regex label) t))))))
   (or sam--buffer-list (buffer-list))))
(defvar sam--other nil "Alist (BUFFER . COMMANDS-WITH-DOT) queued by X and Y.")

(defun sam--change (beg end text)
  (let ((c (list beg end text (cl-incf sam--seq))))
    (push c sam--changes)
    c))

(defun sam--matches (re beg end)
  "All matches of RE in [BEG, END); an empty match advances one char."
  (let ((case-fold-search nil) (more t) out)
    (save-excursion
      (goto-char beg)
      (while (and more (re-search-forward re end t))
        (let ((mb (match-beginning 0)) (me (match-end 0)))
          (push (cons mb me) out)
          (when (= mb me)
            (if (< me end) (forward-char 1) (setq more nil))))))
    (nreverse out)))

(defun sam--shell (cmd input)
  "Run CMD with INPUT; return stdout, or signal `sam-error' on failure."
  ;; 2026-10-20: A failed filter must never become replacement text.  Keep
  ;; stderr separate so diagnostics cannot corrupt a successful replacement.
  (let ((stderr-file (make-temp-file "sam-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (when input (insert input))
          (let* ((status (call-process-region
                          (point-min) (point-max) shell-file-name t
                          (list t stderr-file) nil shell-command-switch cmd))
                 (diagnostic (with-temp-buffer
                               (insert-file-contents stderr-file)
                               (string-trim (buffer-string)))))
            (unless (eq status 0)
              (sam--fail "Shell command failed (%s)%s" status
                         (if (string-empty-p diagnostic) ""
                           (concat ": " diagnostic))))
            (unless (string-empty-p diagnostic)
              (message "Sam shell: %s" diagnostic))
            (buffer-string)))
      (delete-file stderr-file))))

(defun sam--print (s)
  (push s sam--output))

(defun sam--line-of (pos)
  (save-excursion (goto-char pos) (line-number-at-pos)))

(defun sam--eval (cmd dot)
  (pcase-let* ((`(,addr ,op . ,args) cmd)
               (d (sam--resolve addr dot))
               (b (car d)) (e (cdr d)))
    (pcase op
      ('select (push (cons 'range d) sam--results))
      ('a (push (cons 'change (sam--change e e (car args))) sam--results))
      ('i (push (cons 'change (sam--change b b (car args))) sam--results))
      ('c (push (cons 'change (sam--change b e (car args))) sam--results))
      ('d (push (cons 'change (sam--change b e "")) sam--results))
      ('s (pcase-let ((`(,re ,to ,n ,g) args) (k 0))
            (save-excursion
              (dolist (m (sam--matches re b e))
                (cl-incf k)
                (when (or (and g (>= k n)) (= k n))
                  (goto-char (car m))
                  (re-search-forward re (cdr m) t)
                  (sam--change (car m) (cdr m)
                               (sam--expand to (cl-loop for i from 0 to 9
                                                        collect (match-string-no-properties i)))))))
            (push (cons 'range d) sam--results)))
      ('p (let ((text (buffer-substring-no-properties b e)))
            (sam--print (if (string-suffix-p "\n" text) text (concat text "\n"))))
          (push (cons 'range d) sam--results))
      ('= (sam--print (format "%d%s; #%d,#%d" (sam--line-of b)
                              (if (> (sam--line-of (max b (1- e))) (sam--line-of b))
                                  (format ",%d" (sam--line-of (max b (1- e)))) "")
                              (- b (point-min)) (- e (point-min)))))
      ('k (sam--save-mark d))
      ((or 'x 'y)
       (let* ((ms (sam--matches (car args) b e))
              (pieces (if (eq op 'x) ms
                        (let ((prev b) out)
                          (dolist (m ms) (push (cons prev (car m)) out) (setq prev (cdr m)))
                          (push (cons prev e) out)
                          (nreverse out)))))
         (dolist (m pieces)
           (if (cadr args) (sam--eval (cadr args) m)
             (push (cons 'range m) sam--results)))))
      ((or 'g 'v)
       ;; The guard tests the dot's text on its own, so ^ and $ anchor to
       ;; the dot's edges: g/^[0-9]+$/ means "the whole dot is a number".
       (when (eq (eq op 'g)
                 (let ((case-fold-search nil))
                   (and (string-match-p (car args) (buffer-substring-no-properties b e)) t)))
         (if (cadr args) (sam--eval (cadr args) d)
           (push (cons 'range d) sam--results))))
      ((or 'X 'Y)
       (let ((matches (sam--matching-buffers (car args) (eq op 'X))))
         (when sam--loop-observer
           (funcall sam--loop-observer (car args) (eq op 'X) matches))
         (dolist (buffer matches) (push (cons buffer (cadr args)) sam--other))))
      ((or 'm 't)
       (let ((to (cdr (sam--resolve (car args) d)))
             (text (buffer-substring-no-properties b e)))
         (when (and (eq op 'm) (< b to e)) (sam--fail "move into itself"))
         (push (cons 'change (sam--change to to text)) sam--results)
         (when (eq op 'm) (sam--change b e ""))))
      ('| (push (cons 'change (sam--change b e (sam--shell (car args) (buffer-substring-no-properties b e))))
                sam--results))
      ('< (push (cons 'change (sam--change b e (sam--shell (car args) nil))) sam--results))
      ('> (sam--print (sam--shell (car args) (buffer-substring-no-properties b e))))
      ('! (sam--print (sam--shell (car args) nil)))
      ('u (undo (car args)))
      ('block (dolist (c (car args)) (sam--eval c d)))
      (_ (sam--fail "unhandled %s" op)))))

(defun sam--expand (template groups)
  "Expand sam's & and \\1..\\9 in TEMPLATE.  GROUPS holds match strings 0-9."
  (replace-regexp-in-string
   "\\\\\\([0-9&\\\\]\\)\\|&"
   (lambda (m)
     (let ((g (match-string 1 m)))
       (cond ((null g) (or (nth 0 groups) ""))
             ((equal g "&") "&")
             ((equal g "\\") "\\")
             (t (or (nth (string-to-number g) groups) "")))))
   template t t))

(defun sam--apply ()
  "Apply `sam--changes'; return the final positions of changes and a mapper."
  (let* ((asc (sort (copy-sequence sam--changes)
                    (lambda (x y)
                      (or (< (car x) (car y))
                          (and (= (car x) (car y))
                               (let ((xi (= (car x) (cadr x))) (yi (= (car y) (cadr y))))
                                 (or (and xi (not yi))
                                     (and (eq xi yi) (< (nth 3 x) (nth 3 y))))))))))
         (final (make-hash-table :test 'eq))
         (shift 0) (prev nil))
    (dolist (c asc)
      (when (and prev (> (cadr prev) (car c)))
        (sam--fail "changes not in sequence"))
      (puthash c (+ (car c) shift) final)
      (cl-incf shift (- (length (nth 2 c)) (- (cadr c) (car c))))
      (setq prev c))
    (dolist (c (reverse asc))
      (goto-char (car c))
      (delete-region (car c) (cadr c))
      (insert (nth 2 c)))
    ;; Map an original position to its final position.  Changes are sorted
    ;; and disjoint, so their END values ascend: binary-search the changes
    ;; that end before POS, add their prefix sum of deltas, then look at the
    ;; few that end exactly at POS (insertions there count only for a start).
    (let* ((vec (vconcat asc))
           (n (length vec))
           (prefix (make-vector (1+ n) 0)))
      (dotimes (k n)
        (let ((c (aref vec k)))
          (aset prefix (1+ k) (+ (aref prefix k)
                                 (- (length (nth 2 c)) (- (cadr c) (car c)))))))
      (cons final
            (lambda (pos startp)
              (let ((lo 0) (hi n))
                (while (< lo hi)
                  (let ((mid (/ (+ lo hi) 2)))
                    (if (< (cadr (aref vec mid)) pos) (setq lo (1+ mid)) (setq hi mid))))
                (let ((s (aref prefix lo)) (k lo))
                  (while (and (< k n) (= (cadr (aref vec k)) pos))
                    (let ((c (aref vec k)))
                      (when (or (< (car c) (cadr c)) startp)
                        (cl-incf s (- (length (nth 2 c)) (- (cadr c) (car c))))))
                    (cl-incf k))
                  (+ pos s))))))))

(defun sam--address-relative-p (addr)
  "Whether ADDR depends on the incoming dot, matching `sam--resolve'."
  (pcase addr
    ((or 'nil '(dot) `(fwd ,_) `(bwd ,_)) t)
    (`(comma ,a ,b)
     (or (sam--address-relative-p a) (sam--address-relative-p b)))
    ;; Semicolon supplies the resolved left address as the right's dot.
    (`(semi ,a ,_) (sam--address-relative-p a))
    (`(,(or 'plus 'minus) ,a ,b)
     ;; On the right of +/- a line/search is relative to the left address;
     ;; character, end and mark addresses ignore that base instead.
     (and (not (memq (car b) '(char end mark)))
          (sam--address-relative-p a)))
    (_ nil)))

(defun sam--run-in (buffer cmds dots)
  "Evaluate CMDS in BUFFER from DOTS; apply the changes; return final ranges."
  (with-current-buffer buffer
    (when sam--run-observer (funcall sam--run-observer buffer))
    (let ((sam--changes nil) (sam--results nil) (sam--seq 0))
      ;; 2026-10-20: Choose top-level scope before evaluation, including shell
      ;; commands and output. Deduplicating edits afterward would be too late.
      ;; Recursive sam--eval calls still execute once per explicit loop/block.
      (dolist (c cmds)
        (dolist (d (if (sam--address-relative-p (car c)) dots (list (car dots))))
          (sam--eval c d)))
      (let* ((res (reverse sam--results))
             (applied (when sam--changes
                        ;; One undo step for the whole command; roll back on error.
                        (let ((group (prepare-change-group)) (ok nil))
                          (unwind-protect
                              (progn (activate-change-group group)
                                     (prog1 (save-excursion
                                              ;; 2026-10-21: These results already
                                              ;; use post-edit coordinates. Use
                                              ;; Kao's public edit seam so its
                                              ;; command-boundary reconciliation
                                              ;; does not translate them again.
                                              (if (and (bound-and-true-p kao-mode)
                                                       (kao-normal-state-p)
                                                       (fboundp 'kao-edit-keeping-selections))
                                                  (let (result)
                                                    (kao-edit-keeping-selections
                                                     (lambda (_) (setq result (sam--apply))))
                                                    result)
                                                (sam--apply)))
                                       (setq ok t)))
                            (if ok
                                (progn (undo-amalgamate-change-group group)
                                       (accept-change-group group))
                              (cancel-change-group group))))))
             (final (car applied)) (mapper (cdr applied)))
        (mapcar (lambda (r)
                  (pcase r
                    (`(change . ,c)
                     (let ((beg (if final (gethash c final) (car c))))
                       (cons beg (+ beg (length (nth 2 c))))))
                    (`(range ,b . ,e)
                     (if mapper (cons (funcall mapper b t) (funcall mapper e nil))
                       (cons b e)))))
                res)))))

(defun sam--current-dots ()
  (cond
   ((and (bound-and-true-p kao-mode) (fboundp 'kao-get-selections))
    (mapcar (lambda (s) (cons (kao-sel-beg s) (min (point-max) (kao-sel-end s))))
            (kao-get-selections)))
   ((use-region-p) (list (cons (region-beginning) (region-end))))
   (t (list (cons (point) (point))))))

(defun sam--select (ranges)
  (let ((ranges (seq-filter (lambda (r) (<= (point-min) (car r) (cdr r) (point-max))) ranges)))
    (when ranges
      (if (and (bound-and-true-p kao-mode) (fboundp 'kao-set-selections))
          (kao-set-selections
           (mapcar (lambda (r)
                     (let ((b (car r)) (e (max (car r) (1- (cdr r)))))
                       (kao-sel-make :anchor (min b (max (point-min) (1- (point-max))))
                                     :cursor (min e (max (point-min) (1- (point-max)))))))
                   ranges)
           (1- (length ranges)))
        (let ((r (car (last ranges))))
          ;; Mark and point bracket the last range, but the region stays
          ;; inactive: an active region would turn the next `undo' into
          ;; undo-in-region and revert only part of the command.
          (goto-char (cdr r))
          (push-mark (car r) t))))))

;;;###autoload
(defun sam-execute (command &optional buffer initial-dots)
  "Run sam COMMAND in BUFFER (default current).  Return the output text.
Top-level addresses independent of the starting selections (such as ,
or 2) run once. Omitted/dot-relative addresses run per starting selection.
Nested loops and blocks retain their explicit repetitions. Addresses use
the accessible buffer; narrowing is respected.
INITIAL-DOTS optionally supplies integer (BEG . END) ranges for the first
command. Later commands use the preceding command's results even when the
target is hidden; displaying a buffer is not part of program semantics.
Signal `sam-error' on failed shell commands, discarding their stdout and
pending edits in that buffer evaluation.  Earlier completed commands and
external shell side effects are not rolled back."
  (let* ((buffer (or buffer (current-buffer)))
         (sam--output nil)
         (dots (with-current-buffer buffer (or initial-dots (sam--current-dots)))))
    (with-current-buffer buffer
      (unless (and dots (cl-every (lambda (range)
                                  (and (consp range) (integerp (car range))
                                       (integerp (cdr range))
                                       (<= (point-min) (car range) (cdr range) (point-max))))
                                dots))
        (sam--fail "invalid starting ranges")))
    (dolist (cmd (sam-parse command))
      (let ((sam--other nil))
        (with-current-buffer buffer
          (let ((ranges (sam--run-in buffer (list cmd) dots)))
            ;; 2026-10-21: A program's next dot comes from evaluation, not
            ;; visible Kao/region state. Hidden command-buffer targets and
            ;; isolated preview copies must execute the same program.
            (when ranges
              (setq dots ranges)
              (when (eq (window-buffer) buffer) (sam--select ranges)))))
        (dolist (o (reverse sam--other))
          (when (cdr o)
            (with-current-buffer (car o)
              (let ((ranges (sam--run-in (car o) (list (cdr o))
                                         (list (cons (point-min) (point-min))))))
                (when (and ranges (eq (car o) buffer)) (setq dots ranges))))))))
    (let ((out (apply #'concat (reverse sam--output))))
      (unless (or sam--inhibit-output (string-empty-p out))
        (with-current-buffer (get-buffer-create "*sam*")
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert out (if (string-suffix-p "\n" out) "" "\n"))))
        (display-buffer "*sam*"))
      out)))

;;;###autoload
(defun sam (command)
  "Read a sam COMMAND and run it on the current buffer.
Kao selections supply the starting targets. An address independent of them,
such as , for the accessible buffer, runs once. Omitted and relative addresses
run per selection; nested loops retain their deliberate repetitions.
`k' saves a buffer-local range that follows edits; apostrophe targets it.
Insertions at its edges join it; deletion collapses it. `sam-clear-mark'
forgets it. See `my/sam-tutor' for the saved-mark practice lesson.
Shell failures report an error instead of replacing text.
See the vault's Sam Tutor for selection-first examples and a safe
failed-filter exercise. Use `my/sam-tutor' for scope/replay lessons and
`my/kao-practice' for checked exercises (SPC h p)."
  (interactive (list (read-string "sam: " nil 'sam-history)))
  (condition-case err
      (sam-execute command)
    (sam-error (user-error "sam: %s" (cadr err)))))

;;;; acme-style command buffer

(defvar-local sam--target nil "Buffer the `sam-scratch' buffer acts on.")
(defvar-local sam--scratch-buffer nil "Command buffer owned by this target buffer.")

(defvar-keymap sam-scratch-mode-map
  "RET" #'sam-scratch-run-line
  "C-c C-c" #'sam-scratch-run-region
  "C-c C-t" #'sam-scratch-choose-target)

(define-derived-mode sam-scratch-mode text-mode "sam"
  "RET runs a line; C-c C-c runs one selected command block.
C-c C-t opens a command buffer for another target, preserving this recipe.")

;;;###autoload
(defun sam-scratch ()
  "Open this target's own Sam command buffer, preserving earlier recipes.
RET runs a line, C-c C-c a selected block, C-c C-t chooses another target.
The header always names the target. Opening from another buffer never
silently retargets an existing command buffer."
  (interactive)
  (let* ((target (if (derived-mode-p 'sam-scratch-mode) sam--target (current-buffer)))
         (_ (unless (buffer-live-p target) (user-error "sam: target buffer is gone")))
         (buf (with-current-buffer target
                (if (and (buffer-live-p sam--scratch-buffer)
                         (eq (buffer-local-value 'sam--target sam--scratch-buffer) target))
                    sam--scratch-buffer
                  (setq sam--scratch-buffer (generate-new-buffer "*sam-scratch*"))))))
    (with-current-buffer buf
      ;; Seed before the major mode/global Kao hooks, retaining selection
      ;; history without any private Kao reset or buffer erasure.
      (when (= (point-min) (point-max))
        (insert "# RET runs a line; C-c C-c runs selected commands (omit these comments).\n"
                "# ,x/TODO.*/            select every TODO line\n"
                "# ,x/foo/ c/bar/        replace foo with bar\n"
                "# ,x g/error/ p         print lines that contain error\n\n"
                "# 2k saves line 2; 'd later deletes its tracked range.\n"
                "# M-x sam-clear-mark forgets it; SPC h p s opens the lesson.\n\n"))
      (unless (derived-mode-p 'sam-scratch-mode) (sam-scratch-mode))
      (setq sam--target target
            header-line-format
            '(:eval (if (buffer-live-p sam--target)
                        (format "Sam target: %s | RET line, C-c C-c block, C-c C-t target"
                                (buffer-name sam--target))
                      "Sam target is gone | C-c C-t chooses another"))))
    (pop-to-buffer buf)))

(defun sam-scratch-choose-target (buffer)
  "Open BUFFER's command buffer, preserving this target and its recipes."
  (interactive (list (read-buffer "Sam target: " (other-buffer (current-buffer) t) t)))
  (with-current-buffer (get-buffer buffer) (sam-scratch)))

(defun sam-scratch--run (command)
  "Run COMMAND on this command buffer's explicitly recorded target."
  (let ((target sam--target))
    (unless (buffer-live-p target) (user-error "sam: target buffer is gone"))
    (condition-case err
        (let ((win (get-buffer-window target)))
          (if win (with-selected-window win (sam-execute command target))
            (sam-execute command target)))
      (sam-error (user-error "sam: %s" (cadr err))))))

(defun sam-scratch--region ()
  "Return one explicitly selected command range, using public Kao state."
  (cond ((use-region-p) (list (region-beginning) (region-end)))
        ((and (bound-and-true-p kao-mode) (= 1 (length (kao-get-selections))))
         (let* ((selection (car (kao-get-selections)))
                (b (kao-sel-beg selection)) (e (kao-sel-end selection)))
           (unless (> (- e b) 1) (user-error "Select a command block first"))
           (list b e)))
        (t (user-error "Select one contiguous command block first"))))

(defun sam-scratch-run-region (begin end)
  "Run the selected multi-line Sam program against the header's target.
Select commands only, excluding the # comment template. Each completed
command has an Emacs undo group; Kao keeps its native command-boundary
history unit for the program. Earlier completed commands survive errors.
Undo in each edited buffer."
  (interactive (sam-scratch--region))
  (sam-scratch--run (buffer-substring-no-properties begin end)))

(defun sam-scratch-run-line ()
  "Run the sam command on the current line against `sam--target'."
  (interactive)
  (let ((line (string-trim (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))
    ;; 2026-10-21: #0 is a character address, not a template comment.
    (unless (or (string-empty-p line) (string-match-p "\\`#[ \t]" line))
      (sam-scratch--run line))))

(provide 'sam)
;;; sam.el ends here
