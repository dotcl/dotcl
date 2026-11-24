;;; With *print-circle*, what *print-length* and *print-level* cut off is not printed, so what
;;; it shares does not get a label (the expected strings are SBCL's).

(defstruct (pcl-ps (:constructor mk-pcl-ps (a b))) a b)

(deftest print-circle-cut.struct-slots-past-print-length
  (let ((s (copy-seq "str")) (c (list 1 2)) (*print-circle* t) (*print-length* 1) (*print-pretty* nil))
    (list (prin1-to-string (mk-pcl-ps s s))
          (prin1-to-string (mk-pcl-ps c c))
          (prin1-to-string (mk-pcl-ps (mk-pcl-ps s 1) s))))
  ("#S(PCL-PS :A \"str\" ...)" "#S(PCL-PS :A (1 ...) ...)" "#S(PCL-PS :A #S(PCL-PS :A \"str\" ...) ...)"))

(deftest print-circle-cut.struct-slots-printed-still-labelled
  (let ((s (copy-seq "str")) (*print-circle* t) (*print-length* 2) (*print-pretty* nil))
    (prin1-to-string (mk-pcl-ps (list s) s)))
  "#S(PCL-PS :A (#1=\"str\") :B #1#)")

(deftest print-circle-cut.vector-tail-of-dotted-list-past-print-level
  (let ((c (list 1 2)) (*print-circle* t) (*print-pretty* nil))
    (list (let ((*print-level* 2)) (prin1-to-string (cons c (vector (vector c)))))
          (let ((*print-level* 3)) (prin1-to-string (cons c (vector (vector (list c))))))
          (let ((*print-level* 1)) (prin1-to-string (cons c (vector c))))))
  ("((1 2) . #(#))" "((1 2) . #(#(#)))" "(# . #)"))

(deftest print-circle-cut.shared-and-printed-twice-is-labelled
  (let ((c (list 1 2)) (*print-circle* t) (*print-pretty* nil))
    (list (prin1-to-string (cons c (vector c)))
          (let ((*print-level* 3)) (prin1-to-string (cons c (vector (vector c)))))))
  ("(#1=(1 2) . #(#1#))" "(#1=(1 2) . #(#(#1#)))"))
