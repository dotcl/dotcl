;;; A test run that registered no test is not a pass: the summary says NO-TESTS-RAN and the
;;; entry scripts exit 1, as they do for a failure. Each case starts a child process that runs
;;; the framework (and, for the last two, test/regression-files.lisp) on a file made here.

(defvar *tec-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))
(defvar *tec-core* (regression-child-core))
(defvar *tec-framework* (namestring (truename "test/framework.lisp")))
(defvar *tec-files* (namestring (truename "test/regression-files.lisp")))

(defun tec-write (name text)
  (let ((path (namestring (merge-pathnames name (regression-temp-dir/)))))
    (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string text s))
    path))

(defun tec-run (script)
  "Run SCRIPT in a child; (exit-code output)."
  (let ((r (dotcl:run-process *tec-exe* (list "--asm" *tec-core* script))))
    (list (first r) (concatenate 'string (second r) (third r)))))

(defun tec-script (name tests &optional (exit-form "(dotcl:quit (tests-exit-code))"))
  (tec-write name (format nil "(load ~S)~%~A~%(do-tests-summary)~%~A~%" *tec-framework* tests exit-form)))

(defun tec-has (result text) (and (search text (second result)) t))

(deftest-emitting-only tests-exit-code.no-test-is-not-a-pass
  (let ((r (tec-run (tec-script "tec-none.lisp" ""))))
    (list (first r) (tec-has r "NO-TESTS-RAN") (tec-has r "ALL-TESTS-PASSED")))
  (1 t nil))

(deftest-emitting-only tests-exit-code.a-pass-and-a-failure
  (let ((p (tec-run (tec-script "tec-pass.lisp" "(deftest tec-p 1 1)")))
        (f (tec-run (tec-script "tec-fail.lisp" "(deftest tec-f 1 2)"))))
    (list (first p) (tec-has p "ALL-TESTS-PASSED") (first f) (tec-has f "FAILED-TESTS")))
  (0 t 1 t))

(deftest-emitting-only tests-exit-code.allow-none
  (first (tec-run (tec-script "tec-allow.lisp" "" "(dotcl:quit (tests-exit-code :allow-none t))")))
  0)

(deftest-emitting-only tests-exit-code.regression-files
  ;; the real entry script: a file that registers nothing fails the run, a file with a test passes it
  (flet ((run-files (file)
           (tec-run (tec-write "tec-files-run.lisp"
                               (format nil "(dotnet:static \"System.Environment\" \"SetEnvironmentVariable\" \"DOTCL_FILES\" ~S)~%(load ~S)~%"
                                       file *tec-files*)))))
    (let ((empty (tec-write "tec-empty-file.lisp" "(+ 1 1)
"))
          (one (tec-write "tec-one-file.lisp" "(deftest tec-one 1 1)
")))
      (let ((e (run-files empty)) (o (run-files one)))
        (list (first e) (tec-has e "NO-TESTS-RAN") (first o) (tec-has o "ALL-TESTS-PASSED")))))
  (1 t 0 t))
