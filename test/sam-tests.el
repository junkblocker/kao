;;; sam-tests.el --- Tests for sam.el -*- lexical-binding: t; -*-

;; Run: make test-sam

(require 'ert)
(require 'sam)

(defun sam-test--run (text cmd)
  "Run CMD on TEXT in a displayed buffer; return (TEXT SELECTED OUTPUT)."
  (with-current-buffer (get-buffer-create "*sam-test*")
    (switch-to-buffer (current-buffer))
    (erase-buffer)
    (fundamental-mode)
    (transient-mark-mode 1)
    (insert text)
    (goto-char (point-min))
    (deactivate-mark)
    (let ((out (sam-execute cmd)))
      (list (buffer-string)
            (and (mark t) (/= (mark t) (point))
                 (buffer-substring-no-properties (min (mark t) (point)) (max (mark t) (point))))
            out))))

(defmacro sam-test--is (text cmd expected)
  `(should (equal (car (sam-test--run ,text ,cmd)) ,expected)))

;;; Text commands and loops

(ert-deftest sam-x-change ()
  (sam-test--is "foo bar foo" ",x/foo/ c/baz/" "baz bar baz"))

(ert-deftest sam-x-default-is-lines ()
  (sam-test--is "a\nb\n" ",x i/- /" "- a\n- b\n"))

(ert-deftest sam-y-between-matches ()
  (sam-test--is "a,b,c" ",y/,/ c/X/" "X,X,X"))

(ert-deftest sam-g-and-v-guards ()
  (sam-test--is "keep 1\ndrop 2\nkeep 3\n" ",x g/drop/ d" "keep 1\nkeep 3\n")
  (sam-test--is "keep 1\ndrop 2\nkeep 3\n" ",x v/keep/ d" "keep 1\nkeep 3\n"))

(ert-deftest sam-nested-loops ()
  ;; Pike's example: change n to m, but only inside words that start with x.
  (sam-test--is "xan nan xnn" ",x/x[a-z]*/ x/n/ c/m/" "xam nan xmm"))

(ert-deftest sam-append-insert-delete ()
  (sam-test--is "a b" ",x/[ab]/ a/!/" "a! b!")
  (sam-test--is "a b" ",x/[ab]/ i/<>/" "<>a <>b")
  (sam-test--is "a b" ",x/ / d" "ab"))

(ert-deftest sam-substitute ()
  (sam-test--is "k=v k=w" ",s/\\([a-z]\\)=\\([a-z]\\)/\\2=\\1/g" "v=k w=k")
  (sam-test--is "aaa" ",s2/a/b/" "aba")
  (sam-test--is "ab" ",s/b/[&]/" "a[b]"))

(ert-deftest sam-block-runs-in-parallel ()
  (sam-test--is "one\n" ",x/one/ {\ni/(/\na/)/\n}" "(one)\n"))

(ert-deftest sam-parallel-semantics-use-original-positions ()
  ;; Both commands see the original text; the second does not see "XX".
  (sam-test--is "ab" ",x/a/ c/XX/" "XXb"))

(ert-deftest sam-overlap-is-an-error ()
  (should-error (sam-test--run "abc" ",x/b/ {\nc/X/\nd\n}") :type 'sam-error))

(ert-deftest sam-zero-width-matches-terminate ()
  ;; sam.el hangs on this; sam must finish.
  (sam-test--is "a\nb\n" ",x/^/ i/> /" "> a\n> b\n> ")
  (sam-test--is "ab" ",x/x*/ i/./" ".a.b."))

;; \n in a regex is a newline, as in sam.
(ert-deftest sam-regex-newline-escape ()
  (sam-test--is "a\n\n\n\nb\n" ",x/\\n\\n+/ c/\\n\\n/" "a\n\nb\n"))

;; A guard sees only the dot, so ^ and $ anchor to its edges.
(ert-deftest sam-guard-anchors-to-dot ()
  (sam-test--is "a,1,x\nb,22,y\n" ",x y/[,\\n]/ g/^[0-9]+$/ c/N/" "a,N,x\nb,N,y\n"))

;;; Addresses

(ert-deftest sam-line-addresses ()
  (sam-test--is "1\n2\n3\n4\n" "2,3d" "1\n4\n")
  (sam-test--is "1\n2\n3\n" "$-1d" "1\n2\n")
  (sam-test--is "1\n2\n3\n" "1+1d" "1\n3\n"))

(ert-deftest sam-regex-and-char-addresses ()
  (sam-test--is "a\nb\nc\n" "/b/d" "a\n\nc\n")
  (sam-test--is "abc" "#1,#2d" "ac")
  (sam-test--is "a\nx\nb\nx\n" "/x/;+1d" "a\nx\n"))

(ert-deftest sam-move-and-copy ()
  (sam-test--is "1\n2\n3\n" "1t$" "1\n2\n3\n1\n")
  (sam-test--is "1\n2\n3\n" "1m$" "2\n3\n1\n"))

;;; Shell and printing

(ert-deftest sam-failed-filter-preserves-text ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "keep this text")
      (should-error (sam-execute ",|false") :type 'sam-error)
      (should (equal (buffer-string) "keep this text")))))

(ert-deftest sam-shell-failure-discards-stdout-and-reports-stderr ()
  (dolist (operator '("|" "<" ">" "!"))
    (let ((err (should-error
                (sam-test--run "keep me"
                               (concat "," operator
                                       "printf partial; printf diagnostic >&2; exit 7"))
                :type 'sam-error)))
      (should (string-match-p "failed (7): diagnostic" (error-message-string err)))
      (should (equal (with-current-buffer "*sam-test*" (buffer-string)) "keep me")))))

(ert-deftest sam-shell-success-separates-diagnostics ()
  (sam-test--is "before" ",|printf 'after\\n'; printf warning >&2" "after\n")
  ;; Successful empty stdout is an intentional deletion, unlike failure.
  (sam-test--is "before" ",|true" ""))

(ert-deftest sam-shell-failure-aborts-pending-buffer-changes ()
  (should-error (sam-test--run "keep me" ",{\ni/prefix/\n|false\n}")
                :type 'sam-error)
  (should (equal (with-current-buffer "*sam-test*" (buffer-string)) "keep me")))

(ert-deftest sam-pipe-keeps-selection-length ()
  ;; sam.el collapses every match to empty after |.
  (let ((r (sam-test--run "x y" ",x/[a-z]/ |tr a-z A-Z")))
    (should (equal (car r) "X Y"))
    (should (equal (cadr r) "Y"))))

(ert-deftest sam-print-and-address ()
  (should (equal (nth 2 (sam-test--run "a\nb\n" ",x g/b/ p")) "b\n"))
  (should (equal (nth 2 (sam-test--run "id=ann id=bob" ",x/[a-z]+$\\|[a-z]+ / x/[a-z]+/ p")) "ann\nbob\n"))
  (should (equal (nth 2 (sam-test--run "a\nb\n" "2=")) "2; #2,#4")))

(ert-deftest sam-lone-loop-selects-matches ()
  (let ((r (sam-test--run "foo bar foo" ",x/foo/")))
    (should (equal (car r) "foo bar foo"))
    (should (equal (cadr r) "foo"))))

(ert-deftest sam-undo-is-one-step ()
  (with-current-buffer (get-buffer-create "*sam-undo*")
    (switch-to-buffer (current-buffer))
    (erase-buffer) (buffer-enable-undo)
    (insert "a a a") (undo-boundary)
    (sam-execute ",x/a/ c/b/")
    (undo-boundary)
    (should (equal (buffer-string) "b b b"))
    (let ((last-command nil)) (undo))
    (should (equal (buffer-string) "a a a"))))

(ert-deftest sam-no-markers-left ()
  ;; sam.el leaves 2 markers per match; sam leaves none.
  (with-current-buffer (get-buffer-create "*sam-mk*")
    (switch-to-buffer (current-buffer))
    (erase-buffer)
    (dotimes (_ 2000) (insert "foo bar\n"))
    (garbage-collect)
    (let ((t0 (float-time)))
      (sam-execute ",x/foo/ c/baz/")
      (should (< (- (float-time) t0) 2.0)))
    (should (= (how-many "baz" (point-min) (point-max)) 2000))))

(ert-deftest sam-X-over-buffers ()
  (let ((b1 (get-buffer-create "sam-x-one")) (b2 (get-buffer-create "sam-x-two")))
    (with-current-buffer b1 (erase-buffer) (insert "foo"))
    (with-current-buffer b2 (erase-buffer) (insert "foo"))
    (with-temp-buffer (sam-execute "X/sam-x-one/ ,x/foo/ c/bar/"))
    (should (equal (with-current-buffer b1 (buffer-string)) "bar"))
    (should (equal (with-current-buffer b2 (buffer-string)) "foo"))))

(ert-deftest sam-parse-errors ()
  (should-error (sam-parse "Z") :type 'sam-error)
  (should-error (sam-parse ",m") :type 'sam-error))

;;; sam-tests.el ends here
