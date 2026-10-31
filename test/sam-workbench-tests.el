;;; sam-workbench.el --- Public recipe sessions and reviewed apply -*- lexical-binding: t; -*-
(require 'ert)
(require 'kao)
(require 'my-sam-workbench)

;; The package exposes commands; leader bindings belong to the Emacs config.

(defmacro sam-workbench-test/with (&rest body)
  (declare (indent 0))
  `(let ((before (buffer-list)) (my/sam-recipes nil) (my/sam-history nil))
     (save-window-excursion
       (unwind-protect
           (progn
             (switch-to-buffer (generate-new-buffer "sam-workbench-fixture"))
             (text-mode) (insert "foo foo\n") (goto-char 1)
             (buffer-enable-undo) (kao-mode 1)
             ,@body)
         (when transient--prefix (execute-kbd-macro (kbd "C-g")))
         (dolist (buffer (cl-set-difference (buffer-list) before))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(ert-deftest sam-workbench-reruns-find-recipe-and-preserves-native-undo ()
  (sam-workbench-test/with
    (setq my/sam--recipe ",x/foo/ a/!/")
    (call-interactively #'my/sam-workbench)
    (execute-kbd-macro (kbd "r r q"))
    (should-not transient--prefix)
    (should (equal (buffer-string) "foo!! foo!!\n"))
    (execute-kbd-macro (kbd "u"))
    (should (equal (buffer-string) "foo! foo!\n"))
    (execute-kbd-macro (kbd "U"))
    (should (equal (buffer-string) "foo!! foo!!\n"))))

(ert-deftest sam-workbench-tutor-recipe-edits-save-and-replay-match-published-text ()
  (sam-workbench-test/with
    ;; Seed the published fixture before enabling Kao; never erase Kao history.
    (switch-to-buffer (generate-new-buffer "sam-workbench-tutor"))
    (text-mode) (insert "foo bar foo\n") (goto-char 1) (kao-mode 1)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ",x/foo/ a/!/")))
      (call-interactively #'my/sam-workbench)
      (execute-kbd-macro (kbd "e r r q")))
    (should (equal (buffer-string) "foo!! bar foo!!\n"))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ",x/foo/ c/qux/")))
      (call-interactively #'my/sam-workbench)
      (execute-kbd-macro (kbd "e p")))
    (should-not transient--prefix)
    (execute-kbd-macro (kbd "a"))
    (execute-kbd-macro (kbd "q"))
    (should (equal (buffer-string) "qux!! bar qux!!\n"))))

(ert-deftest sam-workbench-preview-suffix-uses-command-buffers-explicit-target ()
  (sam-workbench-test/with
    (setq my/sam--recipe ",x/foo/ c/bar/")
    (let ((target (current-buffer)))
      (sam-scratch)
      (call-interactively #'my/sam-workbench)
      (execute-kbd-macro (kbd "p"))
      (should-not transient--prefix)
      (should (eq major-mode 'my/sam-preview-mode))
      (should (eq (plist-get my/sam--preview-plan :target) target))
      (execute-kbd-macro (kbd "a"))
      (should (equal (with-current-buffer target (buffer-string)) "bar bar\n")))))

(ert-deftest sam-workbench-retargets-explicitly-including-after-target-death ()
  (sam-workbench-test/with
    (let ((original (current-buffer))
          (other (generate-new-buffer "sam-workbench-other")))
      (with-current-buffer other (insert "new target") (text-mode))
      (call-interactively #'my/sam-workbench)
      (kill-buffer original)
      (cl-letf (((symbol-function 'read-buffer) (lambda (&rest _) (buffer-name other))))
        (my/sam-choose-target))
      (should (eq (my/sam--target) other))
      (should (eq (window-buffer (selected-window)) other)))))

(ert-deftest sam-workbench-preview-refuses-target-insert-state-and-consumes-plan ()
  (sam-workbench-test/with
    (setq my/sam--recipe ",c/bar/")
    (let* ((target (current-buffer)) (preview (my/sam-preview-recipe)))
      (switch-to-buffer target)
      (execute-kbd-macro (kbd "i"))
      (switch-to-buffer preview)
      (should-error (my/sam-apply-recipe-preview) :type 'user-error)
      (should-not my/sam--preview-plan)
      (should (equal (with-current-buffer target (buffer-string)) "foo foo\n")))))

(ert-deftest sam-workbench-known-actions-guard-pinned-target-context ()
  (sam-workbench-test/with
    (setq my/sam--recipe ",c/bar/")
    (call-interactively #'my/sam-workbench)
    (let ((original (current-buffer)))
      (switch-to-buffer (generate-new-buffer " *sam-other*"))
      (insert "untouched")
      (my/sam-run-recipe)
      (should (equal (buffer-string) "untouched"))
      (should (equal (with-current-buffer original (buffer-string)) "bar"))
      (with-current-buffer original (fundamental-mode))
      (should-error (my/sam-run-recipe) :type 'user-error)
      (kill-buffer original)
      (should-error (my/sam-run-recipe) :type 'user-error))))

(ert-deftest sam-workbench-quit-and-unknown-keys-release-without-text-edits ()
  (sam-workbench-test/with
    (dolist (quit '("q" "<escape>" "C-g"))
      (let ((before (buffer-string)))
        (call-interactively #'my/sam-workbench)
        (condition-case nil (execute-kbd-macro (kbd "d")) (user-error nil))
        (should transient--prefix)
        (should (equal before (buffer-string)))
        (execute-kbd-macro (kbd quit))
        (should-not transient--prefix)))))

(ert-deftest sam-workbench-named-recipes-save-load-without-execution ()
  ;; Emacs 29/30 use this flag; 31 retains it as an obsolete alias.
  (with-suppressed-warnings ((obsolete savehist-loaded))
  (sam-workbench-test/with
    (let ((savehist-file (make-temp-file "sam-test-history-"))
          (savehist-loaded t))
      (unwind-protect
          (progn
            (setq my/sam--recipe ",x/foo/ c/bar/")
            (my/sam-save-recipe "example")
            (should (file-readable-p savehist-file))
            (setq my/sam--recipe nil)
            (my/sam-load-recipe "example")
            (should (equal my/sam--recipe ",x/foo/ c/bar/"))
            (should (equal (buffer-string) "foo foo\n"))
            (with-temp-buffer
              (insert-file-contents savehist-file)
              (should (search-forward "my/sam-recipes" nil t))))
        (delete-file savehist-file))))))

(ert-deftest sam-workbench-preview-shows-diff-and-applies-once-through-real-key ()
  (sam-workbench-test/with
    (setq my/sam--recipe ",x/foo/ c/bar/")
    (let* ((target (current-buffer)) (preview (my/sam-preview-recipe)))
      (should (eq (current-buffer) preview))
      (should (eq major-mode 'my/sam-preview-mode))
      (should (string-match-p "-foo foo" (buffer-string)))
      (should (string-match-p "+bar bar" (buffer-string)))
      (should (equal (with-current-buffer target (buffer-string)) "foo foo\n"))
      (execute-kbd-macro (kbd "a"))
      (should (equal (with-current-buffer target (buffer-string)) "bar bar\n"))
      (should-error (my/sam-apply-recipe-preview) :type 'user-error))))

(ert-deftest sam-workbench-buffer-loop-run-exits-to-preview-before-any-edit ()
  (sam-workbench-test/with
    (setq my/sam--recipe "X/^sam-workbench-fixture/ ,c/bar/")
    (let ((target (current-buffer)))
      (call-interactively #'my/sam-workbench)
      (execute-kbd-macro (kbd "r"))
      (should-not transient--prefix)
      (should (eq major-mode 'my/sam-preview-mode))
      (should (equal (with-current-buffer target (buffer-string)) "foo foo\n"))
      (execute-kbd-macro (kbd "a"))
      (should (equal (with-current-buffer target (buffer-string)) "bar")))))

(ert-deftest sam-workbench-read-only-narrowed-insert-contexts-are-explicit ()
  (sam-workbench-test/with
    (setq my/sam--recipe ",c/bar/")
    (narrow-to-region 1 4)
    (my/sam-run-recipe)
    (should (equal (buffer-string) "bar"))
    (save-restriction (widen) (should (equal (buffer-string) "bar foo\n")))
    (setq buffer-read-only t)
    (should-error (my/sam-run-recipe))
    (setq buffer-read-only nil)
    (execute-kbd-macro (kbd "i"))
    (should-error (my/sam-run-recipe) :type 'user-error)))

;;; sam-workbench.el ends here
