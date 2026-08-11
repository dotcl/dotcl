;;; An anonymous (NIL-named) restart must still be established.
;;;
;;; CLHS: a (nil () :report ...) restart, and WITH-SIMPLE-RESTART with a NIL name,
;;; are legal. They cannot be found BY NAME, but they are established and they do
;;; appear in COMPUTE-RESTARTS.
;;;
;;; %PUSH-RESTART-CLUSTER tested the spec's name with `pair.Car is Symbol`. NIL is
;;; its own class here rather than a Symbol, so that test was false and the
;;; anonymous restart was DROPPED ENTIRELY — no error, it simply did not exist.
;;; COMPILE-RESTART-CASE emits the name as the string "NIL" and builds the
;;; LispRestart[] itself, so it never went through that code; only the interpreted
;;; path, which takes the macro expansion, was affected
;;; (ansi-test RESTART-CASE.35 / WITH-SIMPLE-RESTART.8).

(defun %ar (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (multiple-value-list (eval form))
      (error (e) (list :error (type-of e))))))

;;; --- an anonymous restart shows up in COMPUTE-RESTARTS (ansi RESTART-CASE.35)

(defparameter %ar-compute
  '(restart-case
    (loop for i from 1 to 4
          for r in (compute-restarts)
          collect (restart-name r))
    (foo () t)
    (bar () t)
    (foo () 'a)
    (nil () :report (lambda (s) (format s "Anonymous restart")) 10)))

(deftest interp-anonymous-restart.compute-restarts-compile
  (%ar :compile %ar-compute)
  ((foo bar foo nil)))

(deftest interp-anonymous-restart.compute-restarts-interpret
  (%ar :interpret %ar-compute)
  ((foo bar foo nil)))

;;; --- an anonymous restart can be invoked (ansi WITH-SIMPLE-RESTART.8)

(defparameter %ar-invoke
  '(with-simple-restart (nil "") (invoke-restart (first (compute-restarts)))))

(deftest interp-anonymous-restart.with-simple-restart-compile
  (%ar :compile %ar-invoke)
  (nil t))

(deftest interp-anonymous-restart.with-simple-restart-interpret
  (%ar :interpret %ar-invoke)
  (nil t))

;;; --- :report works on an anonymous restart too (when dropped there was none)

(deftest interp-anonymous-restart.report-interpret
  (%ar :interpret '(with-output-to-string (s)
                    (restart-case
                        (let ((r (first (compute-restarts))) (*print-escape* nil))
                          (format s "~A" r))
                      (nil () :report (lambda (st) (format st "anon here")) 10))))
  ("anon here"))

;;; --- over-fix guard: named restarts still work

(deftest interp-anonymous-restart.named-still-works-interpret
  (%ar :interpret '(restart-case (invoke-restart 'foo) (foo () :ok)))
  (:ok))

(deftest interp-anonymous-restart.named-with-simple-restart-interpret
  (%ar :interpret '(with-simple-restart (my-r "desc") (invoke-restart 'my-r)))
  (nil t))
