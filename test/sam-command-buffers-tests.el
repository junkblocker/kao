;;; sam-command-buffers.el --- Programs and explicit scratch targets -*- lexical-binding: t; -*-
(require 'ert)
(require 'sam)

(ert-deftest sam-program-hidden-target-keeps-all-result-ranges ()
  (with-temp-buffer
    (insert "foo foo")
    (goto-char 1)
    (sam-execute ",x/foo/ c/bar/\na/!/" (current-buffer))
    (should (equal (buffer-string) "bar! bar!"))))

(ert-deftest sam-scratch-character-address-is-a-command ()
  (save-window-excursion
    (with-temp-buffer
      (insert "a")
      (goto-char 1)
      (let ((target (current-buffer))
            (scratch (generate-new-buffer " *sam-command-test*")))
        (unwind-protect
            (with-current-buffer scratch
              (sam-scratch-mode)
              (setq sam--target target)
              (insert "#0i/intro/\n")
              (goto-char 1)
              (sam-scratch-run-line)
              (should (equal (with-current-buffer target (buffer-string)) "introa")))
          (kill-buffer scratch))))))

(ert-deftest sam-program-explicit-starting-ranges-and-validation ()
  (with-temp-buffer
    (insert "abcdef")
    (sam-execute "c/X/\na/!/" (current-buffer) '((2 . 3) (5 . 6)))
    (should (equal (buffer-string) "aX!cdX!f"))
    (let ((before (buffer-string)))
      (should-error (sam-execute "d" (current-buffer) '((0 . 5))) :type 'sam-error)
      (should (equal (buffer-string) before)))))

(ert-deftest sam-scratch-keeps-recipes-and-targets-separate ()
  (let ((a (generate-new-buffer " *sam-target-a*"))
        (b (generate-new-buffer " *sam-target-b*")) first second)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer a) (insert "A") (sam-scratch)
          (setq first (current-buffer))
          (goto-char (point-max)) (insert ",a/!/\n") (forward-line -1)
          (switch-to-buffer b) (insert "B") (sam-scratch)
          (setq second (current-buffer))
          (should-not (eq first second))
          (should (eq (buffer-local-value 'sam--target first) a))
          (should (eq sam--target b))
          (switch-to-buffer a) (sam-scratch)
          (should (eq (current-buffer) first))
          (sam-scratch-run-line)
          (should (equal (with-current-buffer a (buffer-string)) "A!"))
          (should (equal (with-current-buffer b (buffer-string)) "B")))
      (dolist (buffer (list first second a b))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest sam-scratch-selected-program-runs-through-native-and-kao-regions ()
  (require 'kao)
  (dolist (kao '(nil t))
    (save-window-excursion
      (with-temp-buffer
        (insert "foo foo") (goto-char 1)
        (let ((target (current-buffer))
              (scratch (generate-new-buffer " *sam-program-region*")))
          (unwind-protect
              (with-current-buffer scratch
                (insert ",x/foo/ c/bar/\na/!/\n")
                (sam-scratch-mode)
                (setq sam--target target)
                (goto-char 1)
                (switch-to-buffer scratch)
                (if kao
                    (progn (kao-mode 1)
                           (kao-set-selections
                            (list (kao-sel-make :anchor 1 :cursor (1- (point-max))))))
                  (transient-mark-mode 1)
                  (push-mark (point-max) t t))
                (execute-kbd-macro (kbd "C-c C-c"))
                (should (equal (with-current-buffer target (buffer-string)) "bar! bar!")))
            (kill-buffer scratch)))))))

(ert-deftest sam-scratch-gone-target-errors-without-changing-recipe ()
  (with-temp-buffer
    (insert ",d") (goto-char 1) (sam-scratch-mode)
    (let ((before (buffer-string)))
      (should-error (sam-scratch-run-line) :type 'user-error)
      (should (equal (buffer-string) before)))))

;;; sam-command-buffers.el ends here
