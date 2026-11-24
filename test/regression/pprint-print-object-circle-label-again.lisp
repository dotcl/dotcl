;;; With *print-circle* an object whose PRINT-OBJECT output does not fit where it is queued is
;;; printed again at its column (see pprint-print-object-prints-list-in-list); its first #n= label
;;; must still come out, not a back reference to a label nobody printed.

(defclass pcl-cnt () ((v :initarg :v :initform 0)))

(defmethod print-object ((o pcl-cnt) s) (format s "#<CNT ~A>" (slot-value o 'v)))

(defun %pcl (margin form-fn)
  (let ((*print-pretty* t) (*print-right-margin* margin) (*print-circle* t)
        (o (make-instance 'pcl-cnt :v 9)))
    (prin1-to-string (funcall form-fn o))))

(deftest pprint-print-object-circle-label-again.narrow
  (list (%pcl 10 (lambda (o) (list o o)))
        (%pcl 14 (lambda (o) (list o o (list o))))
        (%pcl 30 (lambda (o) (list o o (list o)))))
  ("(#1=#<CNT 9>
 #1#)"
   "(#1=#<CNT 9>
 #1# (#1#))"
   "(#1=#<CNT 9> #1# (#1#))"))

(deftest pprint-print-object-circle-label-again.nested-first
  (list (%pcl 14 (lambda (o) (list (list o) o o o o))))
  ("((#1=#<CNT 9>)
 #1# #1# #1#
 #1#)"))
