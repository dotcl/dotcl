;;; regression: skip-under-interp
;;; A local function (LABELS / FLET) that a NOTINLINE declaration inside the scope keeps from being
;;; substituted at its calls has to exist as a function object: a lambda that has its own
;;; (DECLARE (VALUES)) is a real closure, and the call it makes under NOTINLINE read the function
;;; from an environment slot that nothing had filled (NIL: "The value is not of type FUNCTION").

(defun %nilc-run (form) (funcall (compile nil form)))

(deftest-compiled-only notinline-local-fn-closure.labels-values-decl
  (%nilc-run '(lambda () (labels ((s (x) x))
                           (funcall (lambda () (declare (values))
                                      (locally (declare (notinline s)) (s 5)))))))
  5)

(deftest-compiled-only notinline-local-fn-closure.flet-values-decl
  (%nilc-run '(lambda () (flet ((s (x) x))
                           (funcall (lambda () (declare (values))
                                      (locally (declare (notinline s)) (s 5)))))))
  5)

(deftest-compiled-only notinline-local-fn-closure.value-used-after
  (%nilc-run '(lambda () (labels ((s (x) x))
                           (funcall (lambda () (declare (values))
                                      (let ((a (s 1))) (locally (declare (notinline s)) (s a))))))))
  1)

(deftest-compiled-only notinline-local-fn-closure.nested-calls
  (%nilc-run '(lambda ()
                (labels ((s (x) (list x)) (u (x) (car x)))
                  (funcall (lambda () (declare (ignorable) (values))
                             (u (u (locally (declare (notinline u)) (u (s (s (locally (declare (notinline s)) (s 5)))))))))))))
  5)

;; Without one of the three it always worked.
(deftest-compiled-only notinline-local-fn-closure.no-values-decl
  (%nilc-run '(lambda () (labels ((s (x) x))
                           (funcall (lambda () (locally (declare (notinline s)) (s 5)))))))
  5)
(deftest-compiled-only notinline-local-fn-closure.no-notinline
  (%nilc-run '(lambda () (labels ((s (x) x))
                           (funcall (lambda () (declare (values)) (s 5))))))
  5)
(deftest-compiled-only notinline-local-fn-closure.direct-call
  (%nilc-run '(lambda () (labels ((s (x) x))
                           ((lambda () (declare (values)) (locally (declare (notinline s)) (s 5)))))))
  5)

;; The function is still substituted when no NOTINLINE for it appears.
(deftest-compiled-only notinline-local-fn-closure.other-name-notinline
  (%nilc-run '(lambda () (labels ((s (x) x))
                           (funcall (lambda () (declare (values))
                                      (locally (declare (notinline car)) (s 5)))))))
  5)
