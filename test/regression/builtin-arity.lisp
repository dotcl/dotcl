;;; Argument-count checks for the C# builtins that indexed the argument array
;;; without checking it.
;;;
;;; Builtins registered through Startup.RegisterUnary/Binary/Ternary already
;;; checked, but ones registered as a raw (new LispFunction(args => ...)) read
;;; args[0] directly, so too few arguments surfaced as a .NET
;;; IndexOutOfRangeException / OverflowException. That is not a Lisp
;;; PROGRAM-ERROR, so (handler-case ... (program-error ...)) could not catch it.
;;;
;;; The compiled path rejects the arity statically, so nothing showed there; it
;;; was only reachable by getting into the function body through the interpreter
;;; (28 ansi-test failures under :interpret).
;;;
;;; Every case asserts both modes — the compiled half records that it was already
;;; correct, the interpreted half is the real gate.

(defun %ba-interpret (form)
  "Evaluate FORM under the interpreter and classify the failure. A PROGRAM-ERROR
   alone is NOT enough to call it fixed: the unchecked path also produced one,
   because the .NET IndexOutOfRange / Overflow gets wrapped on its way out. What
   distinguishes a real check is that the report states the arity — the leaked
   .NET message says \"Index was outside the bounds of the array.\" instead. This
   is the assertion that gates the fix."
  (let ((dotcl:*evaluator-mode* :interpret))
    (handler-case (progn (eval form) :no-error)
      (program-error (c)
        (if (search "too few arguments" (princ-to-string c))
            :arity-error
            (list :leaked (princ-to-string c))))
      (error (e) (list :other (type-of e))))))

(defun %ba-compile (form)
  "The compiled path rejects these calls statically and always did; it is
   asserted only so a future change cannot quietly move the behaviour to one
   path. Its wording differs per call site (and is sometimes absent), so this
   only checks the condition type."
  (let ((dotcl:*evaluator-mode* :compile))
    (handler-case (progn (eval form) :no-error)
      (program-error () :program-error)
      (error (e) (list :other (type-of e))))))

(defmacro def-arity-test (name form)
  `(progn
     (deftest ,(intern (format nil "BUILTIN-ARITY.~a.COMPILE" name))
       (%ba-compile ',form)
       :program-error)
     (deftest ,(intern (format nil "BUILTIN-ARITY.~a.INTERPRET" name))
       (%ba-interpret ',form)
       :arity-error)))

;;; sequences
(def-arity-test "SORT"               (sort))
(def-arity-test "SORT-1"             (sort nil))
(def-arity-test "STABLE-SORT"        (stable-sort))
(def-arity-test "REPLACE"            (replace))
(def-arity-test "REPLACE-1"          (replace nil))
(def-arity-test "COUNT-IF-NOT"       (count-if-not))
(def-arity-test "FIND-IF-NOT"        (find-if-not))
(def-arity-test "POSITION-IF-NOT"    (position-if-not))
(def-arity-test "MEMBER-IF-NOT"      (member-if-not))
(def-arity-test "ASSOC-IF-NOT"       (assoc-if-not))
(def-arity-test "RASSOC-IF-NOT"      (rassoc-if-not))
(def-arity-test "SUBSTITUTE-IF-NOT"  (substitute-if-not 'a))
(def-arity-test "NSUBSTITUTE-IF-NOT" (nsubstitute-if-not 'a))
(def-arity-test "CONCATENATE"        (concatenate))

;;; misc / arrays / characters
(def-arity-test "FUNCALL"            (funcall))
(def-arity-test "EVAL"               (eval))
(def-arity-test "MAKE-ARRAY"         (make-array))
(def-arity-test "DIGIT-CHAR-P"       (digit-char-p))

;;; pathnames / files
(def-arity-test "PATHNAME"           (pathname))
(def-arity-test "NAMESTRING"         (namestring))
(def-arity-test "TRANSLATE-LOGICAL-PATHNAME" (translate-logical-pathname))
(def-arity-test "FILE-AUTHOR"        (file-author))
(def-arity-test "DELETE-FILE"        (delete-file))
(def-arity-test "RENAME-FILE"        (rename-file))

;;; streams
(def-arity-test "WRITE-CHAR"         (write-char))
(def-arity-test "UNREAD-CHAR"        (unread-char))

;;; ------------------------------------------------------------------
;;; The upper bound — pass extra arguments to builtins whose maximum CLHS fixes.
;;;
;;; Adding the lower bound (above) left the upper one unchecked. A raw
;;; LispFunction simply ignores args.Length > n, so surplus arguments were dropped
;;; silently and (pathname "x" nil) returned #P"x". The compiled path rejects the
;;; arity statically, so again nothing showed there (10 *.ERROR.* failures under
;;; :interpret across PATHNAME / NAMESTRING / DIGIT-CHAR-P / EVAL / RANDOM /
;;; WRITE-CHAR / UNREAD-CHAR / READ-LINE / FILE-AUTHOR / DELETE-FILE).
;;;
;;; The assertion is "the report states the arity", for the same reason as the
;;; lower bound. Before the fix these did not signal at all, so they fail as
;;; :NO-ERROR.

(defun %ba-interpret-max (form)
  (let ((dotcl:*evaluator-mode* :interpret))
    (handler-case (progn (eval form) :no-error)
      (program-error (c)
        (if (search "too many arguments" (princ-to-string c))
            :arity-error
            (list :leaked (princ-to-string c))))
      (error (e) (list :other (type-of e))))))

(defmacro def-max-arity-test (name form)
  `(progn
     (deftest ,(intern (format nil "BUILTIN-ARITY.MAX.~a.COMPILE" name))
       (%ba-compile ',form)
       :program-error)
     (deftest ,(intern (format nil "BUILTIN-ARITY.MAX.~a.INTERPRET" name))
       (%ba-interpret-max ',form)
       :arity-error)))

(def-max-arity-test "PATHNAME"      (pathname "x" nil))
(def-max-arity-test "NAMESTRING"    (namestring "x" nil))
(def-max-arity-test "FILE-AUTHOR"   (file-author "x" nil))
(def-max-arity-test "DELETE-FILE"   (delete-file "x" nil))
(def-max-arity-test "DIGIT-CHAR-P"  (digit-char-p #\1 10 'foo))
(def-max-arity-test "EVAL"          (eval nil nil))
(def-max-arity-test "RANDOM"        (random 10 *random-state* nil))
(def-max-arity-test "WRITE-CHAR"    (with-output-to-string (s) (write-char #\a s nil)))
(def-max-arity-test "UNREAD-CHAR"   (with-input-from-string (s "abc")
                                      (read-char s) (unread-char #\a s nil)))
(def-max-arity-test "READ-LINE"     (with-input-from-string (s "ab") (read-line s t nil nil nil)))

;;; RANDOM signalled a plain LispError, not a PROGRAM-ERROR, when given too few
;;; arguments (ansi RANDOM.ERROR.1). Adding the upper bound was the moment to make
;;; its lower bound match the rest.
(def-arity-test "RANDOM"            (random))

;;; valid calls must still be accepted — this change adds errors, so watch the
;;; false-positive side
(deftest builtin-arity.max-valid-calls-still-work
  (let ((dotcl:*evaluator-mode* :interpret))
    (list (eval '(namestring (pathname "x")))
          (eval '(digit-char-p #\a 16))
          (eval '(integerp (random 10 *random-state*)))
          (eval '(with-output-to-string (s) (write-char #\a s)))
          (eval '(with-input-from-string (s "abc") (read-char s) (unread-char #\a s) (read-char s)))
          (eval '(with-input-from-string (s "ab") (read-line s nil :eof nil)))))
  ("x" 10 t "a" #\a "ab"))

(deftest builtin-arity.valid-calls-still-work
  (let ((dotcl:*evaluator-mode* :interpret))
    (list (eval '(sort (list 3 1 2) #'<))
          (eval '(concatenate 'list '(1) '(2)))
          (eval '(count-if-not #'evenp '(1 2 3)))
          (eval '(digit-char-p #\7))
          (eval '(funcall #'+ 1 2))
          (eval '(coerce (make-array 2 :initial-element 0) 'list))
          (eval '(let ((s (copy-seq "abcd"))) (replace s "XY") s))))
  ((1 2 3) (1 2) 2 7 3 (0 0) "XYcd"))
