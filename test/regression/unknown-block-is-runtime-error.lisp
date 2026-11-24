;;; RETURN-FROM a block that is not there, and GO to a tag that is not there, are
;;; reported and become a run-time PROGRAM-ERROR for that form. They used to abort
;;; the compilation of the whole enclosing toplevel form, so a HANDLER-CASE around
;;; them never saw the error and a file with such a DEFUN could not be loaded.

(deftest unknown-block.return-from-in-handler-case
  (eval '(handler-case (return-from ubre-nosuch 1) (error () :caught)))
  :caught)

(deftest unknown-block.loop-named-return
  (eval '(handler-case (loop named ubre-nm repeat 3 do (return 3)) (error () :caught)))
  :caught)

(deftest unknown-block.go-in-handler-case
  (eval '(handler-case (go ubre-nosuch) (error () :caught)))
  :caught)

(deftest unknown-block.defun-still-loads
  (progn
    (let ((*error-output* (make-broadcast-stream)))
      (eval '(defun ubre-bad () (return-from ubre-nosuch 1))))
    (list (handler-case (ubre-bad) (program-error () :program-error))
          (handler-case (ubre-bad) (error () :error))))
  (:program-error :error))

(deftest unknown-block.loop-named-return-clause-ok
  (list (eval '(loop named ubre-nm2 repeat 3 return 1))
        (eval '(loop named ubre-nm3 for x in '(1 2 3) when (> x 1) return x)))
  (1 2))
