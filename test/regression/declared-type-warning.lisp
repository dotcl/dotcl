;;; A type declaration naming something that is not a type is dropped by the
;;; compiler. Silently, until now: a typo, or DECIMAL written bare where
;;; DOTCL:DECIMAL was meant, cost the declaration (and the native arithmetic it
;;; would have enabled) without a word.
;;;
;;; The measurement that justified warning: across ansi-test (836 files, 18944
;;; forms) there are 911 type declarations and zero that name an unknown type,
;;; so this is silent on conforming code.

;;; Every test here probes a COMPILE-TIME diagnostic through EVAL, so all of them
;;; are compiled-only: under :interpret nothing is analysed, which makes the
;;; "warns" cases fail and, the reason none of them may stay a plain DEFTEST,
;;; makes the "stays quiet" cases pass vacuously.

(defun %dtw-warned-p (form)
  "Evaluate FORM, answering whether it signalled a warning."
  (let ((warned nil))
    (handler-bind ((warning (lambda (c)
                              (setf warned t)
                              (muffle-warning c))))
      (eval form))
    warned))

;;; A type nothing defines.
(deftest-compiled-only declared-type-warning-unknown
  (%dtw-warned-p '(defun %dtw-1 (x) (declare (type dtw-no-such-type x)) x))
  t)

;;; The shorthand form, in a LET.
(deftest-compiled-only declared-type-warning-shorthand
  (%dtw-warned-p '(defun %dtw-2 () (let ((a 1)) (declare (dtw-other-missing a)) a)))
  t)

;;; Real types stay quiet: built-in, DEFTYPE, class, and dotcl's own DECIMAL.
(deftest-compiled-only declared-type-warning-builtin-quiet
  (%dtw-warned-p '(defun %dtw-3 (i) (declare (type fixnum i)) (+ i 1)))
  nil)

(deftype %dtw-small () '(integer 0 9))

(deftest-compiled-only declared-type-warning-deftype-quiet
  (%dtw-warned-p '(defun %dtw-4 (i) (declare (type %dtw-small i)) i))
  nil)

(defclass %dtw-thing () ())

(deftest-compiled-only declared-type-warning-class-quiet
  (%dtw-warned-p '(defun %dtw-5 (o) (declare (type %dtw-thing o)) o))
  nil)

(deftest-compiled-only declared-type-warning-dotcl-decimal-quiet
  (%dtw-warned-p '(defun %dtw-6 (x y) (declare (type dotcl:decimal x y)) (+ x y)))
  nil)

;;; ... and the declaration it stayed quiet about actually works.
(deftest-compiled-only declared-type-warning-dotcl-decimal-native
  (princ-to-string (%dtw-6 #m1.50 #m2.25))
  "#m3.75")

;;; A bare DECIMAL is a different symbol, so the compiler's decimal path does not
;;; fire: which is exactly the case worth reporting.
(deftest-compiled-only declared-type-warning-bare-decimal
  (%dtw-warned-p '(defun %dtw-7 (x y) (declare (type decimal x y)) (+ x y)))
  t)

;;; A declaration with no variables is the shape of a user-proclaimed
;;; DECLARATION identifier, not a type shorthand. Proclaimed ones are tracked,
;;; so the two cases separate: an identifier the user declared is honored in
;;; silence, and one nothing declared is reported like any other name the
;;; compiler cannot attach. (ansi-test probes exactly this to decide whether to
;;; run its DECLARATION.4-11 tests, which is why the unproclaimed case must
;;; warn.)
(deftest-compiled-only declared-type-warning-declaration-identifier-quiet
  (progn
    (proclaim '(declaration %dtw-some-declaration))
    (%dtw-warned-p '(eval '(let () (declare (%dtw-some-declaration)) nil))))
  nil)

(deftest-compiled-only declared-type-warning-unproclaimed-declaration
  (%dtw-warned-p '(eval '(let () (declare (%dtw-never-proclaimed)) nil)))
  t)

;;; DECLAIM has to take effect at compile time as well: the rest of the file
;;; being compiled must already see the name, or every use of it is reported.
(deftest-compiled-only declared-type-warning-declaim-is-compile-time
  (%dtw-warned-p '(progn (declaim (declaration %dtw-declaimed))
                         (defun %dtw-10 (x) (declare (%dtw-declaimed)) x)))
  nil)

;;; Reported once per name: the second occurrence is quiet.
(deftest-compiled-only declared-type-warning-reported-once
  (list (%dtw-warned-p '(defun %dtw-8 (x) (declare (type dtw-repeated-type x)) x))
        (%dtw-warned-p '(defun %dtw-9 (x) (declare (type dtw-repeated-type x)) x)))
  (t nil))

;;; The explicit (TYPE spec var) form is a STYLE-WARNING, as SBCL's "undefined
;;; type" is: the type may be defined later in the file, and a full WARNING sets
;;; COMPILE-FILE's failure-p, which ASDF can treat as a failed compile. The
;;; shorthand (spec var) stays a full WARNING, as SBCL's "unrecognized
;;; declaration" is.
(defun %dtw-warning-kind (form)
  "Evaluate FORM, answering :STYLE, :FULL or NIL for the warning it signalled."
  (let ((kind nil))
    (handler-bind ((warning (lambda (c)
                              (setf kind (if (typep c 'style-warning) :style :full))
                              (muffle-warning c))))
      (eval form))
    kind))

(deftest-compiled-only declared-type-warning-type-form-is-style-warning
  (list (%dtw-warning-kind '(defun %dtw-11 (x) (declare (type dtw-later-type x)) x))
        (%dtw-warning-kind '(defun %dtw-12 () (let ((a 1)) (declare (dtw-later-shorthand a)) a))))
  (:style :full))

(deftest-compiled-only declared-type-warning-type-form-not-failure
  (let ((src (regression-temp-file "declared-type-warning-later.lisp")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string "(defun dtw-later (x) (declare (type dtw-defined-later x)) x)
(defclass dtw-defined-later () ())
" s))
    (multiple-value-bind (out warnings-p failure-p)
        (handler-bind ((warning #'muffle-warning))
          (compile-file src))
      (declare (ignore warnings-p))
      (ignore-errors (delete-file out))
      (ignore-errors (delete-file src))
      failure-p))
  nil)
