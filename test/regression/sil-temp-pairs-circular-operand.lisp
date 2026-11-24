;;; The single-use temporary count looks at every operand of an instruction; an operand
;;; that is user data with a cycle in it ends the walk.

(deftest sil-temp-pairs-circular-operand.counts
  (let ((cyc (list 'a 'b)))
    (setf (cddr cyc) cyc)
    (list (dotcl.cil-compiler::%sil-stack-temp-pairs
           `((:declare-local x "LispObject") (:ldc-i4 1) (:stloc x) (:call "Foo" ,cyc) (:ldloc x)))
          (dotcl.cil-compiler::%sil-stack-temp-pairs `((:call "Foo" ,cyc)))))
  (0 0))
