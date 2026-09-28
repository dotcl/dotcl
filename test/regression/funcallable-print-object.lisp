;;; PRINT-OBJECT methods apply to funcallable instances.
;;;
;;; Bug: an instance of a FUNCALLABLE-STANDARD-CLASS is represented as a
;;; GenericFunction carrying the user's class, and the printer printed every
;;; GenericFunction in the built-in #<GENERIC-FUNCTION ...> form without
;;; consulting PRINT-OBJECT. TRY's trial objects (named-readtables' test suite)
;;; printed as #<GENERIC-FUNCTION UNNAMED>.

(defclass %fpo-a () ((n :initarg :n :initform 0))
  (:metaclass dotcl-mop:funcallable-standard-class))

(defmethod print-object ((x %fpo-a) s)
  (print-unreadable-object (x s :type t)
    (format s "n=~a" (slot-value x 'n))))

(deftest funcallable-print-object.prin1
  (prin1-to-string (make-instance '%fpo-a :n 3))
  "#<%FPO-A n=3>")

(deftest funcallable-print-object.princ
  (princ-to-string (make-instance '%fpo-a :n 4))
  "#<%FPO-A n=4>")

(deftest funcallable-print-object.nested
  (format nil "~s" (list 1 (make-instance '%fpo-a :n 5)))
  "(1 #<%FPO-A n=5>)")

;; A method on a superclass applies to the subclass.
(defclass %fpo-b (%fpo-a) ()
  (:metaclass dotcl-mop:funcallable-standard-class))

(deftest funcallable-print-object.inherited
  (prin1-to-string (make-instance '%fpo-b :n 6))
  "#<%FPO-B n=6>")

;; CALL-NEXT-METHOD reaches the default printer and does not re-enter the method.
(defclass %fpo-c () ()
  (:metaclass dotcl-mop:funcallable-standard-class))

(defmethod print-object ((x %fpo-c) s)
  (write-string "<" s) (call-next-method) (write-string ">" s))

(deftest funcallable-print-object.call-next-method
  (let ((s (prin1-to-string (make-instance '%fpo-c))))
    (list (subseq s 0 3) (char s (1- (length s)))))
  ("<#<" #\>))

;; A funcallable class without its own method still prints the default way.
(defclass %fpo-d () ()
  (:metaclass dotcl-mop:funcallable-standard-class))

(deftest funcallable-print-object.no-method
  (subseq (prin1-to-string (make-instance '%fpo-d)) 0 2)
  "#<")
