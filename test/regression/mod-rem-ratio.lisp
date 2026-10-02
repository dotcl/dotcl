;;; MOD and REM of a ratio are the second values of FLOOR and TRUNCATE. The
;;; exact path used the numerators only, so (mod 7/2 1) was 0 and
;;; (rem -7/2 1) was 0.

(deftest mod-rem-ratio
  (list (mod 7/2 1) (rem 7/2 1) (mod -7/2 2) (rem -7/2 1) (mod 5 3/2) (rem -5 3/2)
        (mod 1/3 -1/2) (mod 6/2 2) (rem 10 4) (mod -10 4)
        (equal (multiple-value-list (floor 7/3 1/2)) (list 4 (mod 7/3 1/2))))
  (1/2 1/2 1/2 -1/2 1/2 -1/2 -1/6 1 2 2 t))
