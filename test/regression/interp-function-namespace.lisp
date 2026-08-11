;;; Function names are their own namespace (CLHS 3.1.1), and a function NAME can
;;; be the cons (SETF F). The interpreter kept FLET / LABELS bindings in the same
;;; alist as variables, keyed by the name and looked up with ASSOC's default EQL
;;; test. Three consequences, all fixed together by giving functions their own
;;; key (the same move go tags needed):
;;;
;;;   1. a (SETF F) local function was invisible — a CONS key can never match
;;;      under EQL — however it was referenced;
;;;   2. a VARIABLE whose value happened to be a function answered in operator
;;;      position:  (let ((list #'car)) (list 1 2))  called CAR;
;;;   3. a global MACRO beat a lexical function of the same name, because
;;;      MACROEXPAND-1 runs before the operator is looked up and cannot see ENV
;;;      (CLHS 3.1.2.1.2.4).
;;;
;;; Both evaluator paths are asserted by binding dotcl:*evaluator-mode* around
;;; the EVAL, so this runs under the ordinary compiled harness.

(defmacro %fns-shadowed () :bad)

(defun %fns (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

;;; --- (setf f) local functions (ansi-test FLET.17 / LABELS.17)

(defparameter %fns-setf
  '(flet (((setf %f) (x y) (setf (car y) x)))
     (let ((z (list 1 2))) (setf (%f z) 'a) z)))

(deftest interp-function-namespace.setf-fn-compile (%fns :compile %fns-setf) (a 2))
(deftest interp-function-namespace.setf-fn-interpret (%fns :interpret %fns-setf) (a 2))

(defparameter %fns-setf-sharp
  '(flet (((setf %f) (v obj) (cons v obj))) (funcall #'(setf %f) 1 2)))

(deftest interp-function-namespace.setf-sharpquote-compile
  (%fns :compile %fns-setf-sharp) (1 . 2))
(deftest interp-function-namespace.setf-sharpquote-interpret
  (%fns :interpret %fns-setf-sharp) (1 . 2))

(defparameter %fns-setf-operator
  '(flet (((setf %f) (v obj) (cons v obj))) ((setf %f) 1 2)))

(deftest interp-function-namespace.setf-operator-interpret
  (%fns :interpret %fns-setf-operator) (1 . 2))

;;; --- a lexical function shadows a global macro (ansi-test FLET.73 / LABELS.51)

(deftest interp-function-namespace.flet-beats-macro-compile
  (%fns :compile '(flet ((%fns-shadowed () :good)) (%fns-shadowed))) :good)
(deftest interp-function-namespace.flet-beats-macro-interpret
  (%fns :interpret '(flet ((%fns-shadowed () :good)) (%fns-shadowed))) :good)
(deftest interp-function-namespace.labels-beats-macro-interpret
  (%fns :interpret '(labels ((%fns-shadowed () :good)) (%fns-shadowed))) :good)

;;; The macro must still win when there is NO lexical binding.
(deftest interp-function-namespace.macro-still-applies-interpret
  (%fns :interpret '(%fns-shadowed)) :bad)

;;; --- variables and functions no longer share a namespace

(deftest interp-function-namespace.variable-not-operator-compile
  (%fns :compile '(let ((list #'car)) (list 1 2))) (1 2))
(deftest interp-function-namespace.variable-not-operator-interpret
  (%fns :interpret '(let ((list #'car)) (list 1 2))) (1 2))

;;; --- the ordinary cases must be unchanged

(deftest interp-function-namespace.plain-flet-interpret
  (%fns :interpret '(flet ((f (x) (* x 2))) (f 21))) 42)

(deftest interp-function-namespace.flet-funarg-interpret
  (%fns :interpret '(block done
                      (flet ((%f (x) (return-from done x)))
                        (mapcar #'%f '(good bad)))
                      'bad))
  good)

(deftest interp-function-namespace.labels-mutual-recursion-interpret
  (%fns :interpret '(labels ((evn (n) (if (zerop n) t (od (1- n))))
                             (od (n) (if (zerop n) nil (evn (1- n)))))
                      (list (evn 10) (od 10))))
  (t nil))

(deftest interp-function-namespace.inner-flet-shadows-outer-interpret
  (%fns :interpret '(flet ((f () :outer))
                      (flet ((f () :inner)) (f))))
  :inner)

(deftest interp-function-namespace.global-function-still-found-interpret
  (%fns :interpret '(funcall #'car '(9 8))) 9)
