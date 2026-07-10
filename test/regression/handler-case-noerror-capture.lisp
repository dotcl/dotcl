;;; Regression: a variable mutated inside a handler-case/restart-case clause
;;; body (handler clause OR :no-error clause) must be boxed, because those
;;; bodies are emitted as LAMBDA bodies by the macro expansion. Previously
;;; find-captured-vars-expr walked handler-case/restart-case clause bodies with
;;; in-lambda = nil, so a var mutated only there was seen as mutated-but-not-
;;; captured and skipped boxing — the setf hit a stack copy and was lost.
;;;
;;; This surfaced in the SBCL cross-build: reduce-constants uses
;;;   (handler-case (funcall fun a b) (arithmetic-error () ...)
;;;    (:no-error (v) (setf reduced-value v reduced-p t)))
;;; so (/ x 60 60) folded the constants to 60 instead of 3600 (the :no-error
;;; setf of reduced-value was dropped), miscompiling DECODE-UNIVERSAL-TIME's
;;; timezone as seconds/60 instead of seconds/3600.

;;; :no-error clause mutating outer lexicals
(deftest hc.noerror.setf
  (let ((rv 60) (rp nil))
    (handler-case (* 60 60)
      (arithmetic-error () nil)
      (:no-error (value) (setf rv value rp t)))
    (list rv rp))
  (3600 t))

;;; both a :no-error setf and an error clause present (the reduce-constants shape)
(deftest hc.noerror.with-error-clause
  (let ((reduced-value 60) (reduced-p nil))
    (handler-case (* 60 60)
      (arithmetic-error () (setf reduced-p :err))
      (:no-error (v) (setf reduced-value v reduced-p t)))
    (list reduced-value reduced-p))
  (3600 t))

;;; handler clause body mutating an outer lexical (also a lambda body)
(deftest hc.handler.setf
  (let ((got nil))
    (handler-case (error "boom")
      (error (c) (setf got (and c t))))
    got)
  t)

;;; restart-case clause body mutating an outer lexical
(deftest rc.clause.setf
  (let ((hit nil))
    (restart-case
        (invoke-restart 'r 99)
      (r (v) (setf hit v)))
    hit)
  99)

;;; end-to-end: reduce-constants-style associative fold via handler-case :no-error
(deftest hc.reduce-fold
  (let ((reduced-value nil) (reduced-p nil))
    (dolist (arg '(60 60))
      (let ((value arg))
        (if reduced-value
            (handler-case (* reduced-value value)
              (arithmetic-error () nil)
              (:no-error (v) (setf reduced-value v reduced-p t)))
            (setf reduced-value value))))
    (list reduced-value reduced-p))
  (3600 t))
