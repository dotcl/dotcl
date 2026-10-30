;;; A list, vector or nested list printed with the standard pretty printer
;;; inside a logical block, holding an object whose PRINT-OBJECT method opens
;;; its own logical block and calls FORMAT with ~A / ~S there. The method's
;;; text must stay inside the element, after the list's opening parenthesis.
;;; Expected values are SBCL 2.6.8's.

(defclass %ppsl-node ()
  ((name :initarg :name :initform 'alpha)
   (kids :initarg :kids :initform nil :accessor %ppsl-kids)
   (parent :initform nil :accessor %ppsl-parent)))

(defmethod print-object ((o %ppsl-node) s)
  (if (not *print-pretty*)
      (write-string "#<node>" s)
      (pprint-logical-block (s nil :prefix "#.(" :suffix ")")
        (write-string "ND" s)
        (when (%ppsl-parent o)
          (write-char #\space s)
          (pprint-newline :linear s)
          (pprint-pop)
          (format s "#| ~S of type ~A |#" :parent 'nd))
        (write-char #\space s)
        (pprint-newline :linear s)
        (format s "#| ~A |#" (slot-value o 'name))
        (write-char #\space s)
        (write :children :stream s)
        (write-char #\space s)
        (write-char #\' s)
        (write (%ppsl-kids o) :stream s))))

(defun %ppsl-slash (s x &optional colon at)
  (declare (ignore colon at))
  (write x :stream s))

(defun %ppsl-in-block (fn)
  (with-output-to-string (s)
    (pprint-logical-block (s nil)
      (write-string "P " s)
      (funcall fn s))))

(deftest pprint-print-object-in-standard-list.parent-child
  (let ((*print-pretty* t) (*print-right-margin* 200)
        (c (make-instance '%ppsl-node)) (p (make-instance '%ppsl-node)))
    (setf (%ppsl-parent c) p (%ppsl-kids p) (list c))
    (write-to-string p))
  "#.(ND #| ALPHA |# :CHILDREN '(#.(ND #| :PARENT of type ND |# #| ALPHA |# :CHILDREN 'NIL)))")

(deftest pprint-print-object-in-standard-list.write
  (let ((*print-pretty* t) (*print-right-margin* 200)
        (l (list 1 (make-instance '%ppsl-node) 2)))
    (%ppsl-in-block (lambda (s) (write l :stream s))))
  "P (1 #.(ND #| ALPHA |# :CHILDREN 'NIL) 2)")

(deftest pprint-print-object-in-standard-list.nested-list
  (let ((*print-pretty* t) (*print-right-margin* 200)
        (l (list 1 (make-instance '%ppsl-node) 2)))
    (%ppsl-in-block (lambda (s) (write (list l l) :stream s))))
  "P ((1 #.(ND #| ALPHA |# :CHILDREN 'NIL) 2) (1 #.(ND #| ALPHA |# :CHILDREN 'NIL) 2))")

(deftest pprint-print-object-in-standard-list.vector
  (let ((*print-pretty* t) (*print-right-margin* 200))
    (%ppsl-in-block (lambda (s) (prin1 (vector 1 (make-instance '%ppsl-node)) s))))
  "P #(1 #.(ND #| ALPHA |# :CHILDREN 'NIL))")

(deftest pprint-print-object-in-standard-list.format-directives
  (let ((*print-pretty* t) (*print-right-margin* 200)
        (l (list 1 (make-instance '%ppsl-node) 2)))
    (list (format nil "~@<P ~S~:>" l)
          (format nil "~@<P ~A~:>" l)
          (format nil "~@<P ~W~:>" l)
          (format nil "~@<P ~/%ppsl-slash/~:>" l)))
  ("P (1 #.(ND #| ALPHA |# :CHILDREN 'NIL) 2)"
   "P (1 #.(ND #| ALPHA |# CHILDREN 'NIL) 2)"
   "P (1 #.(ND #| ALPHA |# :CHILDREN 'NIL) 2)"
   "P (1 #.(ND #| ALPHA |# :CHILDREN 'NIL) 2)"))

(deftest pprint-print-object-in-standard-list.break
  (let ((*print-pretty* t) (*print-right-margin* 50)
        (l (list (make-instance '%ppsl-node :name 'alpha)
                 (make-instance '%ppsl-node :name 'beta))))
    (%ppsl-in-block (lambda (s) (write l :stream s))))
  #.(format nil "P (#.(ND #| ALPHA |# :CHILDREN 'NIL)~%   #.(ND #| BETA |# :CHILDREN 'NIL))"))
