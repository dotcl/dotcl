;;; A structure or a class whose only PRINT-OBJECT methods are :around / :before / :after
;;; still goes through them, wrapped around the default printing, as it does in SBCL:
;;; (prin1-to-string (make-pt :x 1)) with an :around that adds brackets is "[#S(PT :X 1)]".
;;; The expected strings were taken from SBCL.

(defstruct poao-pt x)
(defclass poao-cc () ())

(defmethod print-object :around ((o poao-pt) s)
  (write-string "[" s) (call-next-method) (write-string "]" s))
(defmethod print-object :before ((o poao-pt) s) (write-string "<" s))
(defmethod print-object :after ((o poao-pt) s) (write-string ">" s))

(defmethod print-object :around ((o poao-cc) s)
  (write-string "[" s) (call-next-method) (write-string "]" s))

(deftest print-object-auxiliary-only.structure
  (list (prin1-to-string (make-poao-pt :x 1))
        (prin1-to-string (list (make-poao-pt :x 2))))
  ("[<#S(POAO-PT :X 1)>]" "([<#S(POAO-PT :X 2)>])"))

(deftest print-object-auxiliary-only.instance
  (let ((s (prin1-to-string (make-instance 'poao-cc))))
    (list (char s 0) (char s (1- (length s))) (not (null (search "POAO-CC" s)))))
  (#\[ #\] t))
