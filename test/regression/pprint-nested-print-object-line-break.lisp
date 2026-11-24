;;; Objects that print themselves with a logical block and a run-time FORMAT, nested three deep
;;; and long enough to need a line break: FORMAT's flush of staged text must leave the text of
;;; every enclosing open block alone, not only the innermost one.

(defvar *pnpo-ctl* "~a")

(defstruct pnpo-node v kids)

(defmethod print-object ((n pnpo-node) s)
  (pprint-logical-block (s nil :prefix "#<node " :suffix ">")
    (format s *pnpo-ctl* (pnpo-node-v n))
    (when (pnpo-node-kids n)
      (format s " ")
      (pprint-logical-block (s (pnpo-node-kids n) :prefix "(" :suffix ")")
        (loop (princ (pprint-pop) s)
              (pprint-exit-if-list-exhausted)
              (write-char #\Space s)
              (pprint-newline :fill s))))))

(deftest pprint-nested-print-object-line-break.three-deep
  (let ((tree (make-pnpo-node :v 'root
                              :kids (list (make-pnpo-node :v 1)
                                          (make-pnpo-node :v 2 :kids (list (make-pnpo-node :v 3)))))))
    (list (let ((*print-pretty* t) (*print-right-margin* 30)) (prin1-to-string tree))
          (let ((*print-pretty* t) (*print-right-margin* 80)) (prin1-to-string tree))
          (let ((*print-pretty* nil)) (prin1-to-string tree))))
  ("#<node ROOT (#<node 1>
             #<node 2 (#<node 3>)>)>"
   "#<node ROOT (#<node 1> #<node 2 (#<node 3>)>)>"
   "#<node ROOT (#<node 1> #<node 2 (#<node 3>)>)>"))
