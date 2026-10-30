;;; INVOKE-RESTART on a restart object looked the object up again with a NIL
;;; condition, so the restart's test function ran with NIL and hid restarts
;;; meant for a condition. An active object is invokable whatever its test says;
;;; a name still goes through the test, and an object whose extent has been left
;;; is still a CONTROL-ERROR. Each case runs on both evaluators.

(defun %iro (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

(define-condition invoke-restart-object-condition (condition) ())

;; A restart found for a condition whose test accepts it.
(defparameter %iro-found-with-condition
  '(let ((c (make-condition 'invoke-restart-object-condition)))
     (restart-case (invoke-restart (find-restart 'iro-foo c))
       (iro-foo () :test (lambda (x) (typep x 'invoke-restart-object-condition))
                :invoked))))

(deftest invoke-restart-object.found-with-condition-compile
  (%iro :compile %iro-found-with-condition)
  :invoked)

(deftest invoke-restart-object.found-with-condition-interpret
  (%iro :interpret %iro-found-with-condition)
  :invoked)

;; The test would answer NIL now, but the object is already in hand.
(defparameter %iro-test-now-false
  '(let ((ok t))
     (restart-case (let ((r (find-restart 'iro-foo
                                          (make-condition 'invoke-restart-object-condition))))
                     (setq ok nil)
                     (invoke-restart r))
       (iro-foo () :test (lambda (x) (declare (ignore x)) ok)
                :invoked))))

(deftest invoke-restart-object.test-now-false-compile
  (%iro :compile %iro-test-now-false)
  :invoked)

(deftest invoke-restart-object.test-now-false-interpret
  (%iro :interpret %iro-test-now-false)
  :invoked)

;; Found through COMPUTE-RESTARTS in a handler; the test answers NIL for NIL.
(defparameter %iro-from-compute-restarts
  '(restart-case
       (handler-bind ((invoke-restart-object-condition
                        (lambda (c)
                          (let ((r (find 'iro-foo (compute-restarts c)
                                         :key #'restart-name)))
                            (invoke-restart r)))))
         (signal (make-condition 'invoke-restart-object-condition)))
     (iro-foo () :test (lambda (x)
                         (and x (typep x 'invoke-restart-object-condition)))
              :invoked)))

(deftest invoke-restart-object.from-compute-restarts-compile
  (%iro :compile %iro-from-compute-restarts)
  :invoked)

(deftest invoke-restart-object.from-compute-restarts-interpret
  (%iro :interpret %iro-from-compute-restarts)
  :invoked)

;; RESTART-BIND establishes the same kind of object, with :TEST-FUNCTION.
(defparameter %iro-restart-bind
  '(restart-bind ((iro-foo (lambda () :invoked)
                    :test-function (lambda (x)
                                     (and x (typep x 'invoke-restart-object-condition)))))
     (invoke-restart (find-restart 'iro-foo
                                   (make-condition 'invoke-restart-object-condition)))))

(deftest invoke-restart-object.restart-bind-compile
  (%iro :compile %iro-restart-bind)
  :invoked)

(deftest invoke-restart-object.restart-bind-interpret
  (%iro :interpret %iro-restart-bind)
  :invoked)

;; INVOKE-RESTART-INTERACTIVELY takes an object the same way.
(defparameter %iro-interactively
  '(let ((c (make-condition 'invoke-restart-object-condition)))
     (restart-case (invoke-restart-interactively (find-restart 'iro-foo c))
       (iro-foo () :test (lambda (x) (typep x 'invoke-restart-object-condition))
                :interactive (lambda () nil)
                :invoked))))

(deftest invoke-restart-object.interactively-compile
  (%iro :compile %iro-interactively)
  :invoked)

(deftest invoke-restart-object.interactively-interpret
  (%iro :interpret %iro-interactively)
  :invoked)

;; After the RESTART-CASE that established it has returned, the object is dead.
(defparameter %iro-dead-object
  '(let ((r nil))
     (restart-case (setq r (find-restart 'iro-foo))
       (iro-foo () :active))
     (handler-case (progn (invoke-restart r) :no-error)
       (control-error () :control-error))))

(deftest invoke-restart-object.dead-object-compile
  (%iro :compile %iro-dead-object)
  :control-error)

(deftest invoke-restart-object.dead-object-interpret
  (%iro :interpret %iro-dead-object)
  :control-error)

;; The test must not run at invocation time, only when the restart is looked up.
(defparameter %iro-test-must-not-run
  '(restart-case
       (let ((r (find-restart 'iro-foo
                              (make-condition 'invoke-restart-object-condition))))
         (invoke-restart r))
     (iro-foo () :test (lambda (x)
                         (if (typep x 'invoke-restart-object-condition)
                             t
                             (error "test called with ~S" x)))
              :invoked)))

(deftest invoke-restart-object.test-not-run-at-invocation-compile
  (%iro :compile %iro-test-must-not-run)
  :invoked)

(deftest invoke-restart-object.test-not-run-at-invocation-interpret
  (%iro :interpret %iro-test-must-not-run)
  :invoked)

;; The iolib shape: a handler finds the restart for the condition it is handling
;; and invokes the object it got back.
(defparameter %iro-handler-find
  '(restart-case
       (handler-bind ((invoke-restart-object-condition
                        (lambda (c)
                          (invoke-restart (find-restart 'iro-retry c)))))
         (signal (make-condition 'invoke-restart-object-condition)))
     (iro-retry () :test (lambda (c)
                           (typep c 'invoke-restart-object-condition))
                 :retried)))

(deftest invoke-restart-object.handler-find-compile
  (%iro :compile %iro-handler-find)
  :retried)

(deftest invoke-restart-object.handler-find-interpret
  (%iro :interpret %iro-handler-find)
  :retried)
