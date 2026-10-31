;;; sam-mark.el --- Saved Sam ranges across edits -*- lexical-binding: t; -*-

(require 'ert)
(require 'sam)

(ert-deftest sam-mark-follows-preceding-sam-edit ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (sam-execute "2k\n#0i/intro\\n/\n'd")
    (should (equal (buffer-string) "intro\na\nc\n"))))

(ert-deftest sam-mark-follows-ordinary-buffer-edits ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (sam-execute "2k")
    (goto-char (point-min))
    (insert "intro\n")
    (delete-region 1 3)
    (sam-execute "'d")
    (should (equal (buffer-string) "tro\na\nc\n"))))

(ert-deftest sam-mark-includes-boundary-insertions ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (sam-execute "2k")
    (goto-char 5)
    (insert "END")
    (goto-char 3)
    (insert "BEGIN")
    (sam-execute "'d")
    (should (equal (buffer-string) "a\nc\n"))))

(ert-deftest sam-mark-retains-replacement-and-collapses-on-deletion ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (sam-execute "2k\n'c/longer\\n/\n'd")
    (should (equal (buffer-string) "a\nc\n"))
    ;; Both endpoints now coincide. Insertion there becomes the saved range.
    (sam-execute "'a/X/\n'd")
    (should (equal (buffer-string) "a\nc\n"))))

(ert-deftest sam-mark-snapshots-parallel-addresses ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (sam-execute "2k\n'{\ni/</\na/>/\n}")
    (should (equal (buffer-string) "a\n<b\n>c\n"))
    (sam-execute "'d")
    (should (equal (buffer-string) "a\nc\n"))))

(ert-deftest sam-mark-is-buffer-local-and-clears-on-mode-change ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (sam-execute "2k")
    (let ((saved sam--mark))
      (with-temp-buffer
        (insert "other")
        (should-error (sam-execute "'d") :type 'sam-error)
        (sam-execute ",k")
        (sam-clear-mark)
        (should-error (sam-execute "'d") :type 'sam-error))
      (fundamental-mode)
      (should-not (marker-buffer (car saved)))
      (should-not (marker-buffer (cdr saved)))
      (should-error (sam-execute "'d") :type 'sam-error))))

(ert-deftest sam-mark-replacement-detaches-old-pair ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (sam-execute "2k")
    (let ((old sam--mark))
      (dotimes (_ 100) (sam-execute "3k"))
      (should-not (marker-buffer (car old)))
      (should-not (marker-buffer (cdr old)))
      (sam-execute "'d")
      (should (equal (buffer-string) "a\nb\n")))))

(ert-deftest sam-mark-detaches-when-buffer-dies ()
  (let ((saved (with-temp-buffer
                 (insert "text")
                 (sam-execute ",k")
                 sam--mark)))
    (should-not (marker-buffer (car saved)))
    (should-not (marker-buffer (cdr saved)))))

(ert-deftest sam-mark-outside-narrowing-fails-without-editing ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (sam-execute "2k")
    (save-restriction
      (narrow-to-region 5 7)
      (should-error (sam-execute "'d") :type 'sam-error))
    (should (equal (buffer-string) "a\nb\nc\n"))
    (sam-execute "'d")
    (should (equal (buffer-string) "a\nc\n"))))

(ert-deftest sam-mark-survives-rejected-changes ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (sam-execute "2k")
    (should-error (sam-execute "'{\nc/X/\nd\n}") :type 'sam-error)
    (should-error (sam-execute "'{\ni/X/\n|exit 7\n}") :type 'sam-error)
    (sam-execute "'d")
    (should (equal (buffer-string) "a\nc\n"))))

(ert-deftest sam-mark-discovery-and-clear-command ()
  (should (string-match-p "sam-clear-mark" (documentation 'sam)))
  (save-window-excursion
    (with-temp-buffer
      (insert "a\nb\nc\n")
      (sam-execute "2k")
      (call-interactively #'sam-clear-mark)
      (should-error (sam-execute "'d") :type 'sam-error)
      ;; Keep this fresh scratch buffer separate from any existing one.
      (let* ((scratch (generate-new-buffer " *sam-mark-discovery*"))
             (original (symbol-function 'get-buffer-create)))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'get-buffer-create)
                         (lambda (name &rest args)
                           (if (equal name "*sam-scratch*") scratch
                             (apply original name args)))))
                (sam-scratch))
              (should (eq (window-buffer) scratch))
              (should (string-match-p "SPC h p s" (buffer-string)))
              (should (string-match-p "sam-clear-mark" (buffer-string))))
          (kill-buffer scratch))))))

;;; sam-mark.el ends here
