;;; A new class under a custom metaclass is finalized through the
;;; FINALIZE-INHERITANCE generic function, so a metaclass's methods on it run.
;;; Only a redefinition used to go through the generic function: the first
;;; definition finalized the class internally, and a FINALIZE-INHERITANCE :AFTER
;;; method (where clsql computes a view class's key slots) never ran for it.
;;; dotcl finalizes when the class is defined, SBCL at the first MAKE-INSTANCE;
;;; the tests look after a MAKE-INSTANCE, where the two agree. Expected values
;;; were checked against SBCL.

(defvar *fin-log* nil)
(defclass fin-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c fin-meta) (s standard-class)) t)
(defmethod dotcl-mop:finalize-inheritance :after ((c fin-meta))
  (push (list (class-name c)
              (mapcar #'dotcl-mop:slot-definition-name (dotcl-mop:class-slots c)))
        *fin-log*))

(defclass fin-c () ((a :initform 1)) (:metaclass fin-meta))

(deftest finalize-inheritance-new-class.after-method-runs
  (progn (make-instance 'fin-c)
         (reverse *fin-log*))
  ((fin-c (a))))

;; A metaclass that finalizes the class itself from INITIALIZE-INSTANCE: the
;; class is not finalized a second time.
(defvar *fin-log2* nil)
(defclass fin-meta2 (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c fin-meta2) (s standard-class)) t)
(defmethod initialize-instance :after ((c fin-meta2) &key)
  (dotcl-mop:finalize-inheritance c))
(defmethod dotcl-mop:finalize-inheritance :after ((c fin-meta2))
  (push (class-name c) *fin-log2*))

(defclass fin-c2 () ((a :initform 1)) (:metaclass fin-meta2))

(deftest finalize-inheritance-new-class.not-twice
  (progn (make-instance 'fin-c2)
         *fin-log2*)
  (fin-c2))
