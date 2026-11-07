;;; *PRINT-LEVEL* counts a PPRINT-FILL / PPRINT-LINEAR / PPRINT-TABULAR list and a
;;; FORMAT ~< ~:> block as one level, and ~S inside the block prints one deeper.

(defun %pl-run (level thunk)
  (let ((*print-pretty* t) (*print-level* level) (*print-right-margin* 80))
    (with-output-to-string (s) (funcall thunk s))))

(defparameter *%pl-obj* '((a b c) (d e f)))

(deftest pprint-level.fill
  (list (%pl-run 0 (lambda (s) (pprint-fill s *%pl-obj*)))
        (%pl-run 1 (lambda (s) (pprint-fill s *%pl-obj*)))
        (%pl-run 2 (lambda (s) (pprint-fill s *%pl-obj*))))
  ("#" "(# #)" "((A B C) (D E F))"))

(deftest pprint-level.linear-and-tabular
  (list (%pl-run 1 (lambda (s) (pprint-linear s *%pl-obj*)))
        (%pl-run 1 (lambda (s) (pprint-tabular s *%pl-obj*)))
        (%pl-run 0 (lambda (s) (pprint-linear s *%pl-obj*))))
  ("(# #)" "(#               #)" "#"))

(deftest pprint-level.format-block
  (list (%pl-run 1 (lambda (s) (format s "~<(~;~@{~S~^ ~}~;)~:>" *%pl-obj*)))
        (%pl-run 2 (lambda (s) (format s "~<(~;~@{~S~^ ~}~;)~:>" *%pl-obj*)))
        (%pl-run 0 (lambda (s) (format s "~<(~;~@{~S~^ ~}~;)~:>" *%pl-obj*))))
  ("(# #)" "((A B C) (D E F))" "#"))
