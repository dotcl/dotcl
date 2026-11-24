;;; *PRINT-CIRCLE* with a PRINT-OBJECT method that prints through ~@<...~:>
;;; labels only what the method prints more than once.

(defclass pcnb-ev () ((form :initarg :form :reader pcnb-form)))

(defmethod print-object ((e pcnb-ev) s)
  (let ((*print-pretty* t))
    (pprint-logical-block (s nil)
      (format s "~@<~S~:@>" (pcnb-form e)))))

(defclass pcnb-two () ((form :initarg :form :reader pcnb-two-form)))

(defmethod print-object ((e pcnb-two) s)
  (format s "~@<~S ~S~:@>" (pcnb-two-form e) (pcnb-two-form e)))

(deftest print-circle-nested-block.not-shared
  (let ((*print-circle* t) (*print-pretty* nil))
    (format nil "~A" (make-instance 'pcnb-ev :form '(is (equalp 1 '(1 . 2))))))
  "(IS (EQUALP 1 '(1 . 2)))")

(deftest print-circle-nested-block.shared
  (let ((*print-circle* t) (*print-pretty* nil))
    (format nil "~A" (make-instance 'pcnb-two :form (list 'a 'b))))
  "#1=(A B) #1#")
