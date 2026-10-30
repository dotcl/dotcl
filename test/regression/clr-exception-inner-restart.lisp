;;; A raw .NET exception (here one rethrown by DOTNET:AWAIT, which does not
;;; convert it at the call) is signalled before the stack unwinds, so a
;;; HANDLER-BIND handler can reach a restart, CATCH or BLOCK established between
;;; the HANDLER-BIND and the raise point (CLHS 9.1.4). The handler used to get
;;; CONTROL-ERROR: the condition was signalled from the HANDLER-BIND's own
;;; filter, and the transfer could only be taken once those frames were gone.

(defun %cir-faulted-task ()
  (dotnet:static "System.Threading.Tasks.Task" "FromException"
                 (dotnet:new "System.InvalidOperationException" "cir boom")))

(defun %cir-fail ()
  (dotnet:await (%cir-faulted-task)))

(defmacro %cir-invoking ((restart) &body body)
  `(handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart ',restart))))
     ,@body))

(deftest clr-exception-inner-restart.basic
  (%cir-invoking (%cir-retry)
    (restart-case (%cir-fail)
      (%cir-retry () :retried)))
  :retried)

(deftest clr-exception-inner-restart.lisp-error-agrees
  (%cir-invoking (%cir-retry)
    (restart-case (error "cir lisp")
      (%cir-retry () :retried)))
  :retried)

(deftest clr-exception-inner-restart.restart-arguments
  (handler-bind ((error (lambda (c) (invoke-restart '%cir-use (princ-to-string c)))))
    (restart-case (%cir-fail)
      (%cir-use (msg) (and (search "cir boom" msg) :used))))
  :used)

;;; A handler that declines leaves the exception to unwind as before, and the
;;; restart is not taken.
(deftest clr-exception-inner-restart.decline
  (let ((log nil))
    (list (handler-case
              (handler-bind ((error (lambda (c) (declare (ignore c)) (push :declined log))))
                (restart-case (%cir-fail)
                  (%cir-retry () (push :restart log) :retried)))
            (error (c) (and (search "cir boom" (princ-to-string c)) :caught)))
          log))
  (:caught (:declined)))

;;; An inner HANDLER-CASE still takes it first.
(deftest clr-exception-inner-restart.handler-case-inside
  (let ((log nil))
    (list (handler-bind ((error (lambda (c) (declare (ignore c)) (push :outer log))))
            (restart-case
                (handler-case (%cir-fail) (error () :handler-case))
              (%cir-retry () :retried)))
          log))
  (:handler-case nil))

(deftest clr-exception-inner-restart.handler-case-outside
  (handler-case
      (%cir-invoking (%cir-retry)
        (restart-case (%cir-fail)
          (%cir-retry () :retried)))
    (error () :handler-case))
  :retried)

(deftest clr-exception-inner-restart.nested
  (list (%cir-invoking (%cir-outer)
          (restart-case
              (restart-case (%cir-fail)
                (%cir-inner () :inner))
            (%cir-outer () :outer)))
        (%cir-invoking (%cir-inner)
          (restart-case
              (restart-case (%cir-fail)
                (%cir-inner () :inner))
            (%cir-outer () :outer))))
  (:outer :inner))

;;; The handler runs first, then the cleanup forms between the raise point and
;;; the restart, then the restart.
(deftest clr-exception-inner-restart.unwind-order
  (let ((log nil))
    (handler-bind ((error (lambda (c) (declare (ignore c))
                            (push :handler log) (invoke-restart '%cir-retry))))
      (restart-case (unwind-protect (%cir-fail) (push :cleanup log))
        (%cir-retry () (push :restart log))))
    (reverse log))
  (:handler :cleanup :restart))

(deftest clr-exception-inner-restart.retry-loop
  (let ((attempts 0))
    (%cir-invoking (%cir-retry)
      (loop
        (restart-case
            (return (progn (incf attempts)
                           (if (< attempts 3) (%cir-fail) attempts)))
          (%cir-retry () nil)))))
  3)

;;; The same faulted task awaited again is signalled again.
(deftest clr-exception-inner-restart.same-exception-twice
  (let ((task (%cir-faulted-task)))
    (loop repeat 2
          collect (%cir-invoking (%cir-retry)
                    (restart-case (dotnet:await task)
                      (%cir-retry () :retried)))))
  (:retried :retried))

(deftest clr-exception-inner-restart.catch-inside
  (handler-bind ((error (lambda (c) (declare (ignore c)) (throw '%cir-tag :thrown))))
    (catch '%cir-tag (%cir-fail)))
  :thrown)

(defvar *%cir-exit* nil)

(deftest clr-exception-inner-restart.block-inside
  (handler-bind ((error (lambda (c) (declare (ignore c)) (funcall *%cir-exit*))))
    (block %cir-b
      (let ((*%cir-exit* (lambda () (return-from %cir-b :returned))))
        (%cir-fail))))
  :returned)

;;; Raised in a Lisp callback that .NET calls: the restart around the .NET call
;;; and one inside the callback are both reachable.
(defun %cir-for-each (fn)
  (let ((list (dotnet:new "System.Collections.Generic.List`1[System.Object]")))
    (dotnet:invoke list "Add" 1)
    (dotnet:invoke list "ForEach"
                   (dotnet:make-delegate "System.Action`1[System.Object]" fn))
    :done))

(deftest clr-exception-inner-restart.callback-outer-restart
  (%cir-invoking (%cir-retry)
    (restart-case (%cir-for-each (lambda (x) (declare (ignore x)) (%cir-fail)))
      (%cir-retry () :retried)))
  :retried)

(deftest clr-exception-inner-restart.callback-inner-restart
  (%cir-invoking (%cir-retry)
    (%cir-for-each (lambda (x) (declare (ignore x))
                     (restart-case (%cir-fail)
                       (%cir-retry () :retried)))))
  :done)

;;; Declined inside the callback: it reaches the caller as an error, and the
;;; handlers have seen it once.
(deftest clr-exception-inner-restart.callback-decline-once
  (let ((n 0))
    (list (handler-case
              (handler-bind ((error (lambda (c) (declare (ignore c)) (incf n))))
                (%cir-for-each (lambda (x) (declare (ignore x))
                                 (restart-case (%cir-fail)
                                   (%cir-retry () :retried)))))
            (error () :caught))
          n))
  (:caught 1))

(deftest clr-exception-inner-restart.other-thread
  (call-on-small-stack
   (lambda ()
     (%cir-invoking (%cir-retry)
       (restart-case (%cir-fail)
         (%cir-retry () :retried)))))
  :retried)
