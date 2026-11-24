;;; A PRINT-OBJECT method of the program (or a structure's :PRINT-FUNCTION / :PRINT-OBJECT)
;;; runs whatever *PRINT-LEVEL* is.

(defstruct (plupo-s (:print-object (lambda (o s) (format s "{S ~S}" (plupo-s-v o))))) v)
(defstruct (plupo-f (:print-function (lambda (o s d) (declare (ignore d)) (format s "<F ~A>" (plupo-f-v o))))) v)
(defstruct plupo-d x)
(defclass plupo-c () ((a :initarg :a)))
(defmethod print-object ((o plupo-c) s) (format s "[C ~S]" (slot-value o 'a)))

(defun %plupo (x &rest args) (apply #'write-to-string x :escape t :pretty nil args))

(deftest print-level-user-print-object.struct
  (list (%plupo (list (make-plupo-s :v 1)) :level 1)
        (%plupo (list (make-plupo-f :v 1)) :level 1)
        (%plupo (make-plupo-s :v 1) :level 0)
        (%plupo (list (list (make-plupo-s :v 1))) :level 1))
  ("({S 1})" "(<F 1>)" "{S 1}" "(#)"))

(deftest print-level-user-print-object.instance
  (list (%plupo (make-instance 'plupo-c :a 1) :level 0)
        (%plupo (list (make-instance 'plupo-c :a 1)) :level 1))
  ("[C 1]" "([C 1])"))

(deftest print-level-user-print-object.circle-label
  (let ((x (make-plupo-s :v 1)))
    (%plupo (list x x) :level 1 :circle t))
  "(#1={S 1} #1#)")

(deftest print-level-user-print-object.default-printing-still-cut
  (list (%plupo (list (make-plupo-d :x 1)) :level 0)
        (%plupo (list (list (make-plupo-d :x 1))) :level 1))
  ("#" "(#)"))
