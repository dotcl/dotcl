;;; Under *PRINT-CIRCLE*, the separate calls a PRINT-OBJECT method makes on
;;; its stream share the labels of the outermost call: an object printed by
;;; two of them gets #n= at the first and #n# at the second. They used to be
;;; independent, so the object was printed in full twice. The expected
;;; strings are SBCL 2.6.8's.

(defclass pcpo-thunk () ((fn :initarg :fn)))
(defmethod print-object ((x pcpo-thunk) stream) (funcall (slot-value x 'fn) stream))
(defclass pcpo-named () ((name :initarg :name)))
(defmethod print-object ((x pcpo-named) s)
  (print-unreadable-object (x s :type t)
    (format s "~S ~S" (slot-value x 'name) (slot-value x 'name))))

(defparameter *pcpo-sub* (list '1+ 3))
(defparameter *pcpo-form* (list 'is (list 'eql *pcpo-sub* 4)))

(defun pcpo-print (obj)
  (let ((*print-circle* t) (*print-pretty* nil))
    (list (prin1-to-string obj) (format nil "<~S>" obj))))

(deftest print-circle-print-object.across-calls
  (pcpo-print (make-instance 'pcpo-thunk
                             :fn (lambda (s)
                                   (format s "~S where " *pcpo-form*)
                                   (prin1 *pcpo-sub* s)
                                   (format s " = ~S" 4))))
  ("(IS (EQL #1=(1+ 3) 4)) where #1# = 4"
   "<(IS (EQL #1=(1+ 3) 4)) where #1# = 4>"))

(deftest print-circle-print-object.nothing-shared
  (pcpo-print (make-instance 'pcpo-thunk
                             :fn (lambda (s) (format s "~S alone" *pcpo-form*))))
  ("(IS (EQL (1+ 3) 4)) alone" "<(IS (EQL (1+ 3) 4)) alone>"))

(deftest print-circle-print-object.one-format
  (list (pcpo-print (make-instance 'pcpo-thunk
                                   :fn (lambda (s) (format s "~S and ~S" *pcpo-sub* (list *pcpo-sub*)))))
        (pcpo-print (make-instance 'pcpo-named :name (list 'a 'b))))
  (("#1=(1+ 3) and (#1#)" "<#1=(1+ 3) and (#1#)>")
   ("#<PCPO-NAMED #1=(A B) #1#>" "<#<PCPO-NAMED #1=(A B) #1#>>")))

(deftest print-circle-print-object.off
  (let ((*print-circle* nil))
    (prin1-to-string (make-instance 'pcpo-named :name (list 'a 'b))))
  "#<PCPO-NAMED (A B) (A B)>")
