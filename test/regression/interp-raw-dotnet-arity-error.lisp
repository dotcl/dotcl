;;; Builtins that raised an argument-count error as a RAW .NET exception.
;;;
;;; (/) (write) (write-to-string) (parse-namestring) threw
;;; `new Exception(...)` / `ArgumentException`. Those are not Lisp conditions, so
;;; an INTERPRETED HANDLER-CASE cannot catch them: the interpreter's HANDLER-BIND
;;; only pushes a handler cluster, and a raw .NET exception never goes through
;;; SIGNAL. (The compiled path works because compile-handler-case emits a real
;;; try/catch that converts .NET exceptions into conditions.)
;;;
;;; So this is a different axis from the plain arity checks: the observable is not
;;; "does it signal" but "is what it signals catchable as a Lisp condition".
;;; ansi-test's SIGNALS-ERROR puts its handler-case INSIDE the test form, which is
;;; why those failed (/.ERROR.1 / WRITE.ERROR.1 / WRITE-TO-STRING.ERROR.1 /
;;; PARSE-NAMESTRING.ERROR.1).
;;;
;;; The assertion therefore puts HANDLER-CASE INSIDE the form being EVAL'd.
;;; Outside — in the compiled test function — it would catch the .NET exception
;;; too, and gate nothing.

(defun %rde (mode form)
  "FORM carries its own handler-case. Wrapping it again here only makes an
   uncaught escape observable as :ESCAPED."
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :escaped (type-of e))))))

(defmacro def-catchable-test (name form)
  (let ((wrapped `(handler-case (progn ,form :no-error)
                    (program-error () :caught-program-error)
                    (error () :caught-plain-error))))
    `(progn
       (deftest ,(intern (format nil "INTERP-RAW-ARITY.~a.COMPILE" name))
         (%rde :compile ',wrapped)
         :caught-program-error)
       (deftest ,(intern (format nil "INTERP-RAW-ARITY.~a.INTERPRET" name))
         (%rde :interpret ',wrapped)
         :caught-program-error))))

(def-catchable-test "DIVIDE"           (/))
(def-catchable-test "WRITE"            (write))
(def-catchable-test "WRITE-TO-STRING"  (write-to-string))
(def-catchable-test "PARSE-NAMESTRING" (parse-namestring))

;;; valid calls are still accepted
(deftest interp-raw-arity.valid-calls-still-work
  (let ((dotcl:*evaluator-mode* :interpret))
    (list (eval '(write-to-string 12))
          (eval '(with-output-to-string (s) (write 12 :stream s)))
          (eval '(namestring (parse-namestring "x")))
          (eval '(/ 2))
          (eval '(/ 6 3))))
  ("12" "12" "x" 1/2 2))
