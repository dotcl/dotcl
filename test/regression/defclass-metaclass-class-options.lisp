;;; A DEFCLASS class option that is not :METACLASS, :DEFAULT-INITARGS or
;;; :DOCUMENTATION belongs to the metaclass, not to DEFCLASS.
;;;
;;; AMOP passes each such option on as an initarg named by the option and valued
;;; by the option's CDR, unevaluated -- the same shape slot options have. DEFCLASS
;;; used to reject every one of them, so a class under a custom metaclass could
;;; not carry one: contextl writes
;;;
;;;   (defclass root-specializer () () (:metaclass standard-layer-class)
;;;     (original-name . t))
;;;
;;; and got "unknown class option ORIGINAL-NAME".
;;;
;;; The three shapes below are what SBCL passes for the same source, which is
;;; where the CDR rule comes from: a dotted option hands over the CDR itself, a
;;; proper one hands over the list of values.

(defclass dcco-meta (standard-class)
  ((tag :initarg tag :initform :none :reader dcco-tag)))

(defmethod dotcl-mop:validate-superclass ((c dcco-meta) (s standard-class)) t)

;; (name . value) -- the value is the CDR itself
(defclass dcco-dotted () () (:metaclass dcco-meta) (tag . t))

(deftest defclass-dotted-class-option-reaches-the-metaclass
  (dcco-tag (find-class 'dcco-dotted))
  t)

;; (name v1 v2) -- the value is the list of values
(defclass dcco-multi () () (:metaclass dcco-meta) (tag a b))

(deftest defclass-multi-value-class-option-reaches-the-metaclass
  (dcco-tag (find-class 'dcco-multi))
  (a b))

;; (name v) -- still a list, of one element
(defclass dcco-single () () (:metaclass dcco-meta) (tag x))

(deftest defclass-single-value-class-option-is-still-a-list
  (dcco-tag (find-class 'dcco-single))
  (x))

;; Option values are not evaluated, exactly as slot option values are not.
(defvar *dcco-var* :should-not-be-read)

(defclass dcco-unevaluated () () (:metaclass dcco-meta) (tag *dcco-var*))

(deftest defclass-class-option-values-are-not-evaluated
  (dcco-tag (find-class 'dcco-unevaluated))
  (*dcco-var*))

;; A custom metaclass with no extra options keeps working (the other lowering).
(defclass dcco-no-options () () (:metaclass dcco-meta))

(deftest defclass-custom-metaclass-without-options-still-works
  (list (dcco-tag (find-class 'dcco-no-options))
        (class-name (class-of (find-class 'dcco-no-options))))
  (:none dcco-meta))

;; The three standard options stay DEFCLASS's own and must not leak through as
;; initargs -- if one of them were passed on, TAG would be bound to it instead of
;; keeping its initform. :DEFAULT-INITARGS also has to go on working.
(defclass dcco-standard-options ()
  ((s :initarg :s :initform 1))
  (:metaclass dcco-meta)
  (:documentation "doc")
  (:default-initargs :s 2))

(deftest defclass-standard-options-are-not-passed-to-the-metaclass
  (list (dcco-tag (find-class 'dcco-standard-options))
        (slot-value (make-instance 'dcco-standard-options) 's))
  (:none 2))

;; Under STANDARD-CLASS there is no metaclass to hand the option to, so it stays
;; the error it was.
(deftest defclass-unknown-class-option-under-standard-class-still-errors
  (handler-case (progn (eval '(defclass dcco-plain () () (bogus . t))) :no-error)
    (program-error () :program-error))
  :program-error)
