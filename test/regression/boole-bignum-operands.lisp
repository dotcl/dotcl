;;; BOOLE with a bignum operand. BOOLE narrowed every argument to a 64-bit
;;; long, so any operand outside that range failed with ".NET: Value was
;;; either too large or too small for an Int64". LOGAND and friends were fine.
;;; Found by the random integer form test (make test-random-forms).

(defun %bbo (op a b) (boole op a b))

(deftest boole-bignum-operands.all-ops
  (let ((x (+ (expt 2 70) 12345))
        (y (- (expt 3 50))))
    (loop for (op ref) in (list (list boole-clr 0)
                                (list boole-set -1)
                                (list boole-1 x)
                                (list boole-2 y)
                                (list boole-c1 (lognot x))
                                (list boole-c2 (lognot y))
                                (list boole-and (logand x y))
                                (list boole-ior (logior x y))
                                (list boole-xor (logxor x y))
                                (list boole-eqv (logeqv x y))
                                (list boole-nand (lognand x y))
                                (list boole-nor (lognor x y))
                                (list boole-andc1 (logandc1 x y))
                                (list boole-andc2 (logandc2 x y))
                                (list boole-orc1 (logorc1 x y))
                                (list boole-orc2 (logorc2 x y)))
          unless (and (eql (%bbo op x y) ref) (eql (boole op x y) ref))
            collect op))
  nil)

(deftest boole-bignum-operands.random-forms
  ;; The two shapes the random form test reported.
  (list (boole boole-and (ash 1 70) 0)
        (%bbo boole-orc2 288 (* 288 212683940868013804))
        (%bbo boole-and 5 6))
  (0 -61252974969987975297 4))
