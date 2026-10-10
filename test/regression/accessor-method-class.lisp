;;; The methods DEFCLASS makes for :READER / :WRITER / :ACCESSOR slot options
;;; are STANDARD-READER-METHOD and STANDARD-WRITER-METHOD (AMOP), not plain
;;; STANDARD-METHOD. A method written with DEFMETHOD stays STANDARD-METHOD.
;;; Expected values are SBCL 2.6.8's answers.

(defclass %amc-class ()
  ((a :accessor %amc-a :initform 1)
   (b :reader %amc-b :writer %amc-set-b :initform 2)))

(defgeneric %amc-other (x))
(defmethod %amc-other ((x %amc-class)) 3)

(defun %amc-method-kinds (gf)
  (mapcar (lambda (m) (symbol-name (type-of m)))
          (dotcl-mop:generic-function-methods gf)))

(deftest accessor-method-class.reader-and-writer
  (list (%amc-method-kinds #'%amc-a)
        (%amc-method-kinds #'(setf %amc-a))
        (%amc-method-kinds #'%amc-b)
        (%amc-method-kinds #'%amc-set-b))
  (("STANDARD-READER-METHOD") ("STANDARD-WRITER-METHOD")
   ("STANDARD-READER-METHOD") ("STANDARD-WRITER-METHOD")))

(deftest accessor-method-class.defmethod-stays-standard
  (%amc-method-kinds #'%amc-other)
  ("STANDARD-METHOD"))

(deftest accessor-method-class.still-standard-methods
  (let ((m (first (dotcl-mop:generic-function-methods #'%amc-a))))
    (list (typep m 'standard-method)
          (typep m 'dotcl-mop:standard-accessor-method)
          (symbol-name (class-name (class-of m)))
          (dotcl-mop:slot-definition-name
           (dotcl-mop:accessor-method-slot-definition m))))
  (t t "STANDARD-READER-METHOD" a))

(deftest accessor-method-class.accessors-still-work
  (let ((o (make-instance '%amc-class)))
    (setf (%amc-a o) 10)
    (%amc-set-b 20 o)
    (list (%amc-a o) (%amc-b o) (%amc-other o)))
  (10 20 3))

(deftest accessor-method-class.reader-method-class-default
  (let* ((class (find-class '%amc-class))
         (slotd (first (dotcl-mop:class-direct-slots class))))
    (list (symbol-name (class-name (dotcl-mop:reader-method-class class slotd)))
          (symbol-name (class-name (dotcl-mop:writer-method-class class slotd)))))
  ("STANDARD-READER-METHOD" "STANDARD-WRITER-METHOD"))
