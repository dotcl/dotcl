;;; An object whose PRINT-OBJECT method prints a long list, itself inside a list printed with the
;;; pretty printer: the method's list is laid out for the column the object starts at, and breaks at
;;; the right margin as it does when the object is printed on its own.

(defstruct ppl-s v)

(defmethod print-object ((o ppl-s) s)
  (write-string "[" s)
  (prin1 (ppl-s-v o) s)
  (write-string "]" s))

(defclass ppl-c () ((v :initarg :v)))

(defmethod print-object ((o ppl-c) s)
  (write-string "[" s)
  (prin1 (slot-value o 'v) s)
  (write-string "]" s))

(defvar *ppl-list* (list 1 2 3 4 5 6 7 8 9 10))

(defun %ppl (x)
  (let ((*print-pretty* t) (*print-right-margin* 20))
    (prin1-to-string x)))

(deftest pprint-print-object-prints-list-in-list.alone
  (list (%ppl (make-ppl-s :v *ppl-list*)) (%ppl (make-instance 'ppl-c :v *ppl-list*)))
  ("[(1 2 3 4 5 6 7 8 9
  10)]"
   "[(1 2 3 4 5 6 7 8 9
  10)]"))

(deftest pprint-print-object-prints-list-in-list.inside-a-list
  (list (%ppl (list (make-ppl-s :v *ppl-list*)))
        (%ppl (list (make-instance 'ppl-c :v *ppl-list*)))
        (%ppl (list 0 (make-ppl-s :v *ppl-list*) (make-ppl-s :v *ppl-list*))))
  ("([(1 2 3 4 5 6 7 8
   9 10)])"
   "([(1 2 3 4 5 6 7 8
   9 10)])"
   "(0
 [(1 2 3 4 5 6 7 8
   9 10)]
 [(1 2 3 4 5 6 7 8
   9 10)])"))
