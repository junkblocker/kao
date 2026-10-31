;;; my-sam-workbench.el --- Explicit Sam recipes and reviewed replay -*- lexical-binding: t; -*-

;; 2026-10-21: A recipe finds targets again; Kao dot repeats its own action.
;; Pin the target during an entered session, preserve direct Sam commands,
;; and require a fresh text preview for open-buffer loops. No Kao changes.
(require 'sam-preview)
(require 'transient)
(require 'savehist)
(require 'diff)
(declare-function kao-mode "kao-state" (&optional arg))
(declare-function kao-normal-state-p "kao-state" ())

(defvar my/sam-recipes nil "Named Sam recipes persisted by existing savehist.")
(defvar my/sam-history nil "Sam workbench input history.")
(dolist (variable '(my/sam-recipes my/sam-history))
  (add-to-list 'savehist-additional-variables variable))
(defvar-local my/sam--recipe nil)
(defvar-local my/sam--preview-plan nil)

(defun my/sam--target ()
  "Return the pinned target, or the direct command's current Sam target."
  (let* ((scope (transient-scope 'my/sam-workbench))
         (target (if scope (plist-get scope :buffer)
                   (if (derived-mode-p 'sam-scratch-mode) sam--target (current-buffer)))))
    (unless (buffer-live-p target) (user-error "Sam target is gone; choose another buffer"))
    (with-current-buffer target
      (when (and scope (not (eq major-mode (plist-get scope :mode))))
        (user-error "Sam target mode changed; reopen the workbench"))
      (when (and (bound-and-true-p kao-mode) (not (kao-normal-state-p)))
        (user-error "Leave Kao insert state before using the workbench")))
    target))

(defun my/sam--get-recipe (target)
  (or (buffer-local-value 'my/sam--recipe target)
      (user-error "Choose Edit recipe, History or Load named recipe first")))

;;;###autoload
(defun my/sam-edit-recipe ()
  "Edit the pinned target's recipe without running it.
For a multiline program, use the Command buffer action and C-c C-c."
  (interactive)
  (let* ((target (my/sam--target))
         (recipe (read-string "Sam recipe (not run yet): "
                              (buffer-local-value 'my/sam--recipe target) 'my/sam-history)))
    (sam-command-operations recipe)
    (with-current-buffer target (setq my/sam--recipe recipe))))

;;;###autoload
(defun my/sam-recipe-history ()
  "Choose a previous recipe for this target; execution is a separate action."
  (interactive)
  (let* ((target (my/sam--target))
         (recipe (completing-read "Sam recipe history: " my/sam-history nil t)))
    (with-current-buffer target (setq my/sam--recipe recipe))))

;;;###autoload
(defun my/sam-save-recipe (name)
  "Save this target's recipe under NAME using existing savehist storage."
  (interactive (list (read-string "Save recipe as: ")))
  (when (string-empty-p (string-trim name)) (user-error "Give the recipe a name"))
  (let ((recipe (my/sam--get-recipe (my/sam--target))))
    (when (and (assoc name my/sam-recipes)
               (not (equal recipe (cdr (assoc name my/sam-recipes))))
               (not (y-or-n-p (format "Replace recipe %s? " name))))
      (user-error "Recipe was not replaced"))
    (setf (alist-get name my/sam-recipes nil nil #'equal) recipe)
    (savehist-save)
    (message "Saved Sam recipe %s; load it in any target buffer" name)))

;;;###autoload
(defun my/sam-load-recipe (name)
  "Choose named recipe NAME for this target, without executing it."
  (interactive (list (completing-read "Load Sam recipe: " my/sam-recipes nil t)))
  (let ((recipe (cdr (assoc name my/sam-recipes))))
    (unless recipe (user-error "No saved recipe named %s" name))
    (with-current-buffer (my/sam--target) (setq my/sam--recipe recipe))))

;;;###autoload
(defun my/sam-run-recipe ()
  "Run this target's recipe again, recomputing its targets from current text.
X/Y buffer loops open a text preview instead; use Apply there explicitly.
Single-buffer shell commands run only on this explicit action. Earlier
completed commands and external effects survive failures; undo per buffer."
  (interactive)
  (let* ((target (my/sam--target)) (recipe (my/sam--get-recipe target)))
    (if (cl-intersection (sam-command-operations recipe) '(X Y))
        (my/sam-preview-recipe)
      (sam-execute recipe target))))

(defvar my/sam-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'my/sam-apply-recipe-preview)
    (define-key map (kbd "q") #'quit-window)
    map))
(define-derived-mode my/sam-preview-mode special-mode "Sam preview"
  "Review changed buffers and text. a applies; q closes without edits.
Apply rechecks buffer text, selections, scope, regex history and targets.
Normal hooks/text properties can still reject or alter actual edits. Undo
is per command and buffer; a later error does not undo earlier commands.")

;;;###autoload
(defun my/sam-preview-recipe ()
  "Review the pinned target's text-only recipe, without executing shell/undo.
This is a preview of the real evaluator on copies, not a second parser.
Selection-only programs can be run directly; their text diff is empty."
  (interactive)
  (let* ((target (my/sam--target))
         (plan (sam-preview (my/sam--get-recipe target) target))
         (inhibit-modification-hooks t)
         (buffer (generate-new-buffer " *Sam recipe preview*")))
    (with-current-buffer buffer
      (my/sam-preview-mode)
      (when (bound-and-true-p kao-mode) (kao-mode -1))
      (setq my/sam--preview-plan plan)
      (let ((inhibit-read-only t) (changed 0))
        (insert (format "Target: %s\nRecipe: %s\n\n"
                        (buffer-name target) (plist-get plan :command)))
        (insert "a: Apply reviewed recipe   q: Close without applying\n")
        (insert "Changed context requires a fresh preview. Undo in each edited buffer.\n")
        (insert "Hooks/protected text are checked by actual edits, not simulated.\n\n")
        (dolist (snapshot (plist-get plan :snapshots))
          (unless (equal (plist-get snapshot :before) (plist-get snapshot :after))
            (cl-incf changed)
            (insert (format "Buffer: %s\n" (plist-get snapshot :name)))
            (let ((old (generate-new-buffer " *Sam before*"))
                  (new (generate-new-buffer " *Sam after*"))
                  (diff-buffer (generate-new-buffer " *Sam diff*")))
              (unwind-protect
                  (progn
                    (with-current-buffer old (insert (plist-get snapshot :before)))
                    (with-current-buffer new (insert (plist-get snapshot :after)))
                    (diff-no-select old new "-u" t diff-buffer)
                    (insert-buffer-substring diff-buffer))
                (let (kill-buffer-hook kill-buffer-query-functions)
                  (kill-buffer old) (kill-buffer new) (kill-buffer diff-buffer))))
            (insert "\n")))
        (insert (format "%d buffer(s) change text; %d buffer(s) evaluated.\n"
                        changed (length (plist-get plan :snapshots))))
        (unless (string-empty-p (plist-get plan :output))
          (insert "\nRecipe output:\n" (plist-get plan :output))))
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun my/sam-apply-recipe-preview ()
  "Apply this preview once, after checking its target snapshots.
A failed or stale attempt consumes the plan; review again before retrying."
  (interactive)
  (unless my/sam--preview-plan (user-error "No unapplied recipe preview in this buffer"))
  (let ((plan my/sam--preview-plan))
    (setq my/sam--preview-plan nil)
    (let ((target (plist-get plan :target)))
      (when (and (buffer-live-p target)
                 (with-current-buffer target
                   (and (bound-and-true-p kao-mode) (not (kao-normal-state-p)))))
        (user-error "Sam target entered insert state; leave it and preview again")))
    (sam-apply-preview plan)
    (message "Applied Sam recipe; undo per edited buffer")))

;;;###autoload
(defun my/sam-choose-target ()
  "Open a new session for an explicitly chosen live target buffer."
  (interactive)
  (switch-to-buffer (read-buffer "Sam target buffer: " (buffer-name (current-buffer)) t))
  (my/sam-workbench (current-buffer)))

;;;###autoload
(defun my/sam-open-command-buffer ()
  "Open the pinned target's own Sam command buffer; keep other recipes."
  (interactive)
  (let ((target (my/sam--target)))
    (switch-to-buffer target)
    (sam-scratch)))

;;;###autoload
(defun my/sam-workbench-help ()
  "Explain recipe replay, scope, safety, cancellation and recovery."
  (interactive)
  (with-help-window "*Sam workbench help*"
    (princ "Sam workbench - SPC s or M-x my/sam-workbench\n\n")
    (princ "Choose text before acting. A buffer may have no file. SPC means Space;\nC-c s means Ctrl+c then s; RET means Enter; M-x means Alt+x.\n\n")
    (princ "Edit/load only chooses a recipe. r runs it against the named target.\nRun again finds targets again: ,x/foo/ c/bar/ searches the accessible buffer.\nWithout an address, commands act on current selections. Narrowing limits scope.\nKao dot still repeats its native action; this menu does not change it.\n\n")
    (princ "p previews text commands. X/Y open-buffer loops always require preview\nand explicit a in the preview buffer. Shell and undo cannot be previewed;\nuse direct C-c s/C-c S deliberately for those multi-buffer programs.\nPreview applies once and refuses changed context. Hook/protected-text errors\ncan still happen during actual apply. Earlier commands/buffers survive errors.\n\n")
    (princ "q, Escape or C-g leaves the menu. Kao u undoes text in each edited\nbuffer; U redoes it. Earlier shell side effects cannot be undone.\nC-c S opens a per-target command buffer: RET runs its current line,\nC-c C-c runs one selected multiline block; exclude instructional comments.\nNamed recipes use existing savehist; they are data, not background jobs.\n\n")
    (princ "SPC h p s opens a disposable Sam Tutor; SPC h p g opens the Guide.\n")))

(defun my/sam--summary ()
  (condition-case err
      (let ((target (my/sam--target)))
        (with-current-buffer target
          (format "Sam target %s | accessible %d..%d | %d selection(s)\nRecipe: %s"
                  (buffer-name) (point-min) (point-max) (length (sam--current-dots))
                  (or my/sam--recipe "choose Edit / History / Load first"))))
    (user-error (error-message-string err))))

(defun my/sam--run-menu-action ()
  "Leave the session for multi-buffer review; keep ordinary replay repeatable."
  (if (cl-intersection (sam-command-operations (my/sam--get-recipe (my/sam--target))) '(X Y))
      (transient--do-exit)
    (transient--do-call)))

;; 2026-10-31: A plain command autoload keeps package discovery independent
;; of Transient macro expansion and defers the optional menu until use.
;;;###autoload (autoload 'my/sam-workbench "my-sam-workbench" "Edit and replay Sam recipes on an explicit target." t)
(transient-define-prefix my/sam-workbench (&optional buffer)
  "Edit, inspect, save and rerun Sam recipes on an explicit target (`SPC s')."
  :transient-non-suffix 'transient--do-warn
  :refresh-suffixes t
  [:description my/sam--summary
   ["Recipe (does not run)"
    ("e" "Edit recipe" my/sam-edit-recipe :transient t)
    ("h" "History" my/sam-recipe-history :transient t)
    ("s" "Save named recipe" my/sam-save-recipe :transient t)
    ("l" "Load named recipe" my/sam-load-recipe :transient t)]
   ["Run / inspect"
    ("r" "Run again (X/Y: preview)" my/sam-run-recipe :transient my/sam--run-menu-action)
    ("p" "Text preview (a applies there)" my/sam-preview-recipe)
    ("c" "Command buffer (RET / C-c C-c)" my/sam-open-command-buffer)
    ("t" "Choose target buffer" my/sam-choose-target)]
   ["Learn / exit"
    ("?" "Scope, repeat and recovery help" my/sam-workbench-help)
    ("q" "Close (also C-g)" transient-quit-one)
    ("<escape>" "Close menu" transient-quit-one)]]
  (interactive)
  (let ((target (or buffer (my/sam--target))))
    (transient-setup 'my/sam-workbench nil nil
                     :scope (list :buffer target :mode (buffer-local-value 'major-mode target)))))

(provide 'my-sam-workbench)
;;; my-sam-workbench.el ends here
