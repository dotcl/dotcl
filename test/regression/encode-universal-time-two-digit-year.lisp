;;; ENCODE-UNIVERSAL-TIME with a two-digit year takes the year within 50 years of the current
;;; one (CLHS). A year exactly 50 ahead counts as 50 behind (SBCL), so the window is
;;; the current year - 50 .. + 49. The result is checked against the current year, so the
;;; test does not date itself.

(defun %eutdy-year (y)
  (nth-value 5 (decode-universal-time (encode-universal-time 0 0 0 1 1 y 0) 0)))

(defun %eutdy-current ()
  (nth-value 5 (get-decoded-time)))

(deftest encode-universal-time-two-digit-year.within-window
  (let ((cur (%eutdy-current)))
    (loop for y from 0 to 99
          always (let ((full (%eutdy-year y)))
                   (and (= (mod full 100) y) (<= (- cur 50) full (+ cur 49))))))
  t)

(deftest encode-universal-time-two-digit-year.boundary
  (let* ((cur (%eutdy-current))
         (ahead50 (mod (+ cur 50) 100))
         (ahead49 (mod (+ cur 49) 100)))
    (list (= (%eutdy-year ahead50) (- (+ cur 50) 100))
          (= (%eutdy-year ahead49) (+ cur 49))))
  (t t))

(deftest encode-universal-time-two-digit-year.four-digit-years-unchanged
  (list (%eutdy-year 1976) (%eutdy-year 2076) (%eutdy-year 1999))
  (1976 2076 1999))
