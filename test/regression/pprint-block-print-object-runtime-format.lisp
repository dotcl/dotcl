;;; A PRINT-OBJECT method that opens a logical block and writes into it with FORMAT, printed from
;;; inside another logical block, used to fail with "Index was out of range": FORMAT's flush of
;;; the buffers staged for an outer stream cleared the text of the inner block's own buffer.
;;; The control string is not a constant here, so FORMAT runs at run time (a constant one is
;;; expanded when compiling and never reached the flush).

(defvar *pbpo-ctl* "~a")

(defstruct pbpo-node v kids)

(defmethod print-object ((n pbpo-node) s)
  (pprint-logical-block (s nil :prefix "#<" :suffix ">")
    (format s *pbpo-ctl* (pbpo-node-v n))
    (when (pbpo-node-kids n)
      (format s " ")
      (pprint-logical-block (s (pbpo-node-kids n) :prefix "(" :suffix ")")
        (loop (princ (pprint-pop) s)
              (pprint-exit-if-list-exhausted)
              (write-char #\Space s)
              (pprint-newline :fill s))))))

(deftest pprint-block-print-object-runtime-format.in-outer-block
  (list (with-output-to-string (o)
          (pprint-logical-block (o nil :prefix "(" :suffix ")")
            (princ (make-pbpo-node :v 1) o)))
        (with-output-to-string (o)
          (pprint-logical-block (o nil :prefix "[" :suffix "]")
            (prin1 (make-pbpo-node :v 2) o)
            (write-char #\Space o)
            (princ (make-pbpo-node :v 3) o))))
  ("(#<1>)" "[#<2> #<3>]"))

(deftest pprint-block-print-object-runtime-format.nested-nodes
  (list (princ-to-string (make-pbpo-node :v 1 :kids (list (make-pbpo-node :v 2)
                                                          (make-pbpo-node :v 3 :kids (list (make-pbpo-node :v 4))))))
        (let ((*print-pretty* t))
          (with-output-to-string (o)
            (pprint-logical-block (o nil :prefix "<" :suffix ">")
              (princ (make-pbpo-node :v 1 :kids (list (make-pbpo-node :v 2))) o)))))
  ("#<1 (#<2> #<3 (#<4>)>)>" "<#<1 (#<2>)>>"))
