;;; Condition system regression tests (D34, D174 etc.)

;;; handler-case catches error
(deftest handler-case-basic
  (handler-case (error "boom")
    (error (e) :caught))
  :caught)

;;; handler-case catches specific condition type
(deftest handler-case-type-error
  (handler-case (let ((x 42)) (check-type x string))
    (type-error () :type-error))
  :type-error)

;;; handler-case returns value when no error
(deftest handler-case-no-error
  (handler-case (+ 1 2)
    (error () :bad))
  3)

;;; handler-case catches .NET exceptions (D34)
(deftest handler-case-dotnet-exception
  (handler-case
      (let ((v (make-array 3)))
        (aref v 10))
    (error () :caught))
  :caught)

;;; nested handler-case — innermost matching handler wins
(deftest handler-case-nested
  (handler-case
      (handler-case (error "inner")
        (type-error () :type-error))
    (error () :error))
  :error)

;;; unwind-protect cleanup runs on error
(deftest unwind-protect-cleanup-on-error
  (let ((cleaned nil))
    (handler-case
        (unwind-protect
             (error "oops")
          (setq cleaned t))
      (error () nil))
    cleaned)
  t)

;;; unwind-protect cleanup runs on normal exit
(deftest unwind-protect-cleanup-normal
  (let ((cleaned nil))
    (unwind-protect
         42
      (setq cleaned t))
    cleaned)
  t)

;;; restart-case / invoke-restart
(deftest restart-case-basic
  (restart-case
      (invoke-restart 'my-restart 99)
    (my-restart (v) (* v 2)))
  198)

;;; make-instance of condition subclass (D174)
(deftest condition-make-instance
  (let ((e (make-instance 'simple-error
                          :format-control "test ~a"
                          :format-arguments '(42))))
    (eq (type-of e) 'simple-error))
  t)

;;; handler-bind
(deftest handler-bind-basic
  (let ((caught nil))
    (handler-bind ((error (lambda (e) (setq caught t) (continue))))
      (with-simple-restart (continue "continue")
        (signal (make-condition 'error))))
    caught)
  t)

;;; restart-case: compute-restarts with condition arg returns non-abort restart
;;; Reproduces DEFPACKAGE.24/25 failure pattern (D696)
(deftest restart-case-compute-restarts-with-condition
  (catch 'handled
    (handler-bind
        ((error (lambda (c)
                  (throw 'handled
                         (if (position 'abort (compute-restarts c)
                                       :key #'restart-name :test-not #'eq)
                             'success
                             'fail)))))
      (restart-case
          (error "test error")
        (continue () :report "Continue" nil))))
  success)

;;; throw inside eval propagates to outer catch (D696)
;;; The eval boundary must not intercept CatchThrowException for tags established outside
(deftest throw-across-eval-boundary
  (catch 'my-tag
    (eval '(throw 'my-tag 42)))
  42)

;;; DEFPACKAGE.24/25 pattern: restart-case inside eval, outer handler-bind + catch
(deftest defpackage-restart-case-across-eval
  (catch 'handled
    (handler-bind
        ((error (lambda (c)
                  (throw 'handled
                         (if (position 'abort (compute-restarts c)
                                       :key #'restart-name :test-not #'eq)
                             'success
                             'fail)))))
      (eval '(restart-case
                  (error "test error from eval")
                (continue () :report "Continue" nil)))))
  success)
