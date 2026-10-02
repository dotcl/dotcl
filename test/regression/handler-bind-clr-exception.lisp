;;; HANDLER-BIND and a raw .NET exception (an InvalidCastException from
;;; compiled code, say). HANDLER-BIND used to catch the exception at its own
;;; frame and signal from there, so its handlers ran only after the frames that
;;; raised it had unwound: no backtrace of the error, the dynamic bindings of the
;;; raise point already undone, and a TYPE-ERROR handler never matched because a
;;; failed cast became a PROGRAM-ERROR. The exception is now signalled from an
;;; exception filter, before the unwind, and a failed cast is a TYPE-ERROR.

(defvar *%hbc-var* :outer)

;;; Safety 0 with a fixnum declaration unboxes without a check, so NIL raises
;;; the CLR's InvalidCastException wherever this DEFUN is compiled (the compiled
;;; and interpret runs; DEFUN is compiled under :INTERPRET too). On the
;;; emit-free build it is interpreted and (+ NIL 1) signals an ordinary
;;; TYPE-ERROR, which the assertions that hold in every mode also accept; the
;;; ones that only a raw .NET exception exercises are DEFTEST-EMITTING-ONLY.
(defun %hbc-raise-cast (x)
  (declare (optimize (safety 0) (speed 3)) (fixnum x))
  (+ x 1))

(defun %hbc-raise ()
  (%hbc-raise-cast nil))

(defun %hbc-middle (thunk)
  (let ((*%hbc-var* :inner))
    (funcall thunk)))

(defmacro %hbc-first-handler ((type &optional (raise '#'%hbc-raise)) &body handler-body)
  "Run RAISE under HANDLER-BIND for TYPE; the handler's value (with E
bound to the condition) is returned by a non-local exit."
  `(block %hbc
     (handler-bind ((,type (lambda (e)
                             (declare (ignorable e))
                             (return-from %hbc (progn ,@handler-body)))))
       (%hbc-middle ,raise)
       :not-signalled)))

(deftest handler-bind-clr-exception.error-handler
  (%hbc-first-handler (error) (typep e 'error))
  t)

(deftest handler-bind-clr-exception.type-error-handler
  (%hbc-first-handler (type-error) (typep e 'type-error))
  t)

;;; The handler runs in the dynamic environment of the raise point.
(deftest handler-bind-clr-exception.dynamic-environment
  (%hbc-first-handler (error) *%hbc-var*)
  :inner)

(deftest handler-bind-clr-exception.handler-case-agrees
  (handler-case (%hbc-middle #'%hbc-raise)
    (type-error () :type-error)
    (error () :other-error))
  :type-error)

;;; Every handler is called once, innermost first, and an unhandled one still
;;; reaches an outer HANDLER-CASE.
(deftest handler-bind-clr-exception.decline-then-handler-case
  (let ((log nil))
    (list (handler-case
              (handler-bind ((error (lambda (e) (declare (ignore e)) (push :outer log))))
                (handler-bind ((error (lambda (e) (declare (ignore e)) (push :inner log))))
                  (%hbc-middle #'%hbc-raise)))
            (type-error () :caught))
          (reverse log)))
  (:caught (:inner :outer)))

;;; A THROW and a restart established outside the HANDLER-BIND are reached.
(deftest handler-bind-clr-exception.throw-out
  (catch '%hbc-tag
    (handler-bind ((error (lambda (e) (declare (ignore e)) (throw '%hbc-tag :thrown))))
      (%hbc-middle #'%hbc-raise)))
  :thrown)

(deftest handler-bind-clr-exception.restart-outside
  (with-simple-restart (%hbc-skip "skip")
    (handler-bind ((error (lambda (e) (declare (ignore e)) (invoke-restart '%hbc-skip))))
      (%hbc-middle #'%hbc-raise))
    :not-signalled)
  nil t)

;;; A restart established between the HANDLER-BIND and the raise point is gone
;;; by the time the transfer can be taken: a CONTROL-ERROR, not an exception
;;; that nothing receives.
(deftest-emitting-only handler-bind-clr-exception.restart-inside
  (handler-case
      (handler-bind ((error (lambda (e) (declare (ignore e)) (invoke-restart '%hbc-skip))))
        (with-simple-restart (%hbc-skip "skip")
          (%hbc-middle #'%hbc-raise)))
    (control-error () :control-error))
  :control-error)

;;; A THROW to a CATCH between the HANDLER-BIND and the raise point: the same.
(deftest-emitting-only handler-bind-clr-exception.catch-inside
  (handler-case
      (handler-bind ((error (lambda (e) (declare (ignore e)) (throw '%hbc-in :x))))
        (catch '%hbc-in
          (%hbc-middle #'%hbc-raise)))
    (control-error () :control-error))
  :control-error)

;;; The failed cast is a TYPE-ERROR naming FIXNUM (the datum is not known).
(deftest-emitting-only handler-bind-clr-exception.cast-expected-type
  (%hbc-first-handler (type-error)
    (list (type-error-datum e) (type-error-expected-type e)))
  (nil fixnum))

;;; The frames that raised it are still on the stack when the handler runs.
(deftest-emitting-only handler-bind-clr-exception.backtrace
  (%hbc-first-handler (error)
    (let ((bt (dotcl:backtrace)))
      (list (and (member "%HBC-RAISE-CAST" bt :test #'string=) t)
            (and (member "%HBC-MIDDLE" bt :test #'string=) t))))
  (t t))
