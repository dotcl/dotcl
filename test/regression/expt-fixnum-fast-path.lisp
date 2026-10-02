;;; EXPT of a fixnum to a non-negative fixnum power is computed on int64 and
;;; falls back to bignum arithmetic when a product would leave that range.
;;; The results at and around the boundary have to match the exact ones.

(defun expt-by-multiplication (b e)
  (let ((r 1)) (dotimes (i e r) (setf r (* r b)))))

(deftest expt-fixnum-fast-path-boundaries
  (list (expt 3 2) (expt -2 63) (expt -2 64) (expt 2 62) (expt 2 63)
        (expt 10 18) (expt 10 19) (expt 0 0) (expt 0 5) (expt -1 1001)
        (expt 7 0) (expt 3037000499 2) (expt 3037000500 2) (expt -3037000500 2)
        (expt 1 most-positive-fixnum) (expt 2 -2))
  (9 -9223372036854775808 18446744073709551616 4611686018427387904
   9223372036854775808 1000000000000000000 10000000000000000000 1 0 -1
   1 9223372030926249001 9223372037000250000 9223372037000250000 1 1/4))

(deftest expt-fixnum-fast-path-agrees-with-multiplication
  (let ((bad nil))
    (dolist (b '(-3 -2 -1 0 1 2 3 10 -10 65535 -65536 2147483647 -2147483648
                 4611686018427387903 -4611686018427387904) bad)
      (dolist (e '(0 1 2 3 5 7 13 31 32 62 63 64 65))
        (unless (= (expt b e) (expt-by-multiplication b e))
          (push (list b e) bad)))))
  nil)
