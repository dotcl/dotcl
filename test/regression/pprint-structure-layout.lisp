;;; A structure printed by default is a logical block with a linear break before each
;;; slot when it does not fit. Expected strings are the output of another implementation (SBCL).

(defstruct pps1 aa bb (cc nil))
(defstruct (pps2 (:include pps1)) dd)

(defun %pst-run (margin level length)
  (let ((*print-pretty* t) (*print-right-margin* margin) (*print-level* level)
        (*print-length* length) (*print-lines* nil) (*print-circle* nil)
        (o (make-pps2 :aa '(1 2 3) :bb "str"
                      :cc (make-pps1 :aa 'x :bb '(a b c d e f g)) :dd #(1 2 3))))
    (with-output-to-string (s) (write o :stream s))))

(defmacro %pst-cases (&rest cases)
  `(progn
     ,@(loop for (margin level length expected) in cases
             for n from 1
             collect `(deftest ,(intern (format nil "PPRINT-STRUCT.~D" n))
                        (%pst-run ,margin ,level ,length)
                        ,expected))))

(%pst-cases
  (14 nil nil "#S(PPS2
   :AA (1 2
        3)
   :BB \"str\"
   :CC #S(PPS1
          :AA X
          :BB (A
               B
               C
               D
               E
               F
               G)
          :CC NIL)
   :DD #(1 2
         3))")
  (14 nil 2 "#S(PPS2
   :AA (1 2
        ...)
   :BB \"str\"
   ...)")
  (14 nil 4 "#S(PPS2
   :AA (1 2
        3)
   :BB \"str\"
   :CC #S(PPS1
          :AA X
          :BB (A
               B
               C
               D
               ...)
          :CC NIL)
   :DD #(1 2
         3))")
  (14 1 nil "#S(PPS2
   :AA #
   :BB \"str\"
   :CC #
   :DD #)")
  (14 1 2 "#S(PPS2
   :AA #
   :BB \"str\"
   ...)")
  (14 1 4 "#S(PPS2
   :AA #
   :BB \"str\"
   :CC #
   :DD #)")
  (14 2 nil "#S(PPS2
   :AA (1 2
        3)
   :BB \"str\"
   :CC #S(PPS1
          :AA X
          :BB #
          :CC NIL)
   :DD #(1 2
         3))")
  (14 2 2 "#S(PPS2
   :AA (1 2
        ...)
   :BB \"str\"
   ...)")
  (14 2 4 "#S(PPS2
   :AA (1 2
        3)
   :BB \"str\"
   :CC #S(PPS1
          :AA X
          :BB #
          :CC NIL)
   :DD #(1 2
         3))")
  (40 nil nil "#S(PPS2
   :AA (1 2 3)
   :BB \"str\"
   :CC #S(PPS1
          :AA X
          :BB (A B C D E F G)
          :CC NIL)
   :DD #(1 2 3))")
  (40 nil 2 "#S(PPS2 :AA (1 2 ...) :BB \"str\" ...)")
  (40 nil 4 "#S(PPS2
   :AA (1 2 3)
   :BB \"str\"
   :CC #S(PPS1
          :AA X
          :BB (A B C D ...)
          :CC NIL)
   :DD #(1 2 3))")
  (40 1 nil "#S(PPS2 :AA # :BB \"str\" :CC # :DD #)")
  (40 1 2 "#S(PPS2 :AA # :BB \"str\" ...)")
  (40 1 4 "#S(PPS2 :AA # :BB \"str\" :CC # :DD #)")
  (40 2 nil "#S(PPS2
   :AA (1 2 3)
   :BB \"str\"
   :CC #S(PPS1 :AA X :BB # :CC NIL)
   :DD #(1 2 3))")
  (40 2 2 "#S(PPS2 :AA (1 2 ...) :BB \"str\" ...)")
  (40 2 4 "#S(PPS2
   :AA (1 2 3)
   :BB \"str\"
   :CC #S(PPS1 :AA X :BB # :CC NIL)
   :DD #(1 2 3))")
  (80 nil nil "#S(PPS2
   :AA (1 2 3)
   :BB \"str\"
   :CC #S(PPS1 :AA X :BB (A B C D E F G) :CC NIL)
   :DD #(1 2 3))")
  (80 nil 2 "#S(PPS2 :AA (1 2 ...) :BB \"str\" ...)")
  (80 nil 4 "#S(PPS2
   :AA (1 2 3)
   :BB \"str\"
   :CC #S(PPS1 :AA X :BB (A B C D ...) :CC NIL)
   :DD #(1 2 3))")
  (80 1 nil "#S(PPS2 :AA # :BB \"str\" :CC # :DD #)")
  (80 1 2 "#S(PPS2 :AA # :BB \"str\" ...)")
  (80 1 4 "#S(PPS2 :AA # :BB \"str\" :CC # :DD #)")
  (80 2 nil "#S(PPS2 :AA (1 2 3) :BB \"str\" :CC #S(PPS1 :AA X :BB # :CC NIL) :DD #(1 2 3))")
  (80 2 2 "#S(PPS2 :AA (1 2 ...) :BB \"str\" ...)")
  (80 2 4 "#S(PPS2 :AA (1 2 3) :BB \"str\" :CC #S(PPS1 :AA X :BB # :CC NIL) :DD #(1 2 3))"))
