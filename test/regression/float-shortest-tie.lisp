;;; When the shortest digit string ends in an exact tie (the value is exactly halfway between two
;;; strings that both read back), the larger last digit is printed, as SBCL does; .NET's
;;; shortest form keeps the even one.

(deftest float-shortest-tie.double-prin1
  (list (prin1-to-string 922126456.0351562d0)
        (prin1-to-string 13191469.618164062d0)
        (prin1-to-string 922126456.03515625d0))
  ("9.221264560351563d8" "1.3191469618164063d7" "9.221264560351563d8"))

(deftest float-shortest-tie.double-f
  (list (format nil "~F" 922126456.0351562d0)
        (format nil "~F" 13191469.618164062d0))
  ("922126456.0351563" "13191469.618164063"))

(deftest float-shortest-tie.single
  (list (prin1-to-string (coerce 498404.624531d0 'single-float))
        (prin1-to-string (coerce -304895.63968953595d0 'single-float))
        (prin1-to-string 498404.625f0))
  ("498404.63" "-304895.63" "498404.63"))

(deftest float-shortest-tie.round-trips
  ;; the larger digit still reads back as the same float
  (list (eql (read-from-string (prin1-to-string 922126456.03515625d0)) 922126456.03515625d0)
        (eql (read-from-string (prin1-to-string 498404.625f0)) 498404.625f0))
  (t t))

(deftest float-shortest-tie.not-a-tie-unchanged
  (list (prin1-to-string 0.1d0) (prin1-to-string 1.5d0) (prin1-to-string 123.456d0)
        (prin1-to-string 1d23) (prin1-to-string 0.1f0) (prin1-to-string 16777216f0)
        (prin1-to-string 0.3d0) (prin1-to-string 100d0) (prin1-to-string 1.0d-5))
  ("0.1d0" "1.5d0" "123.456d0" "1.0d23" "0.1" "1.6777216e7" "0.3d0" "100.0d0" "1.0d-5"))

(deftest float-shortest-tie.odd-last-digit-unchanged
  ;; an exact tie whose smaller string ends in an odd digit already has the larger one chosen
  (list (prin1-to-string 0.5d0) (prin1-to-string 2.5d0) (prin1-to-string 0.0078125d0)
        (prin1-to-string 1.0009765625d0))
  ("0.5d0" "2.5d0" "0.0078125d0" "1.0009765625d0"))
