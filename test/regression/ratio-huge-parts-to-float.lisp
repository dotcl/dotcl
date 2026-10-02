;;; A ratio whose numerator and denominator are both beyond the float range
;;; (but whose value is ordinary) was taken to a float by converting the two
;;; parts separately: infinity / infinity, NaN. That happened in single-float
;;; contagion (a ratio times, plus or divided by a single-float) once the parts
;;; passed 3.4e38, and in FORMAT ~F / ~E once they passed 1.8e308. Found by
;;; ansi-test's random type propagation tests (RANDOM-TYPE-PROP./.7).

(defvar *rhpf-r* (/ (expt 10 50) (+ 3 (expt 10 50))))
(defvar *rhpf-d* (/ (expt 10 400) (+ 1 (expt 10 400))))

(deftest ratio-huge-parts-to-float
  (list (* *rhpf-r* 2.0) (/ *rhpf-r* 2.0) (+ *rhpf-r* 2.0) (- 2.0 *rhpf-r*)
        (format nil "~,3F" *rhpf-d*) (format nil "~,2E" *rhpf-d*))
  (2.0 0.5 3.0 1.0 "1.000" "1.00e+0"))
