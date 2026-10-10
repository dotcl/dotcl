;;; SOME returns the predicate's first true result as one value, and EVERY,
;;; NOTANY and NOTEVERY return one value. They returned right after the last
;;; predicate call with its values still current, so
;;; (multiple-value-list (some f ...)) saw all of them (SBCL: one value).

(defun %spv-p (x) (values (and (> x 1) x) :extra))

(deftest some-primary-value
  (list (multiple-value-list (some #'%spv-p (list 1 2)))
        (multiple-value-list (some #'%spv-p (vector 1 2)))
        (multiple-value-list (some (lambda (c) (values (char= c #\b) :extra)) "ab"))
        (multiple-value-list (some (lambda (x y) (values (and (> x 1) y) :extra))
                                   (list 1 2) (list 3 4)))
        (multiple-value-list (funcall 'some #'%spv-p (list 1 2)))
        (multiple-value-list (some (lambda (x) (values (and (> x 1) x) :extra)) (list 1 2)))
        (multiple-value-list (some #'%spv-p (list 0 1))))
  ((2) (2) (t) (4) (2) (2) (nil)))

(defun %spv-q (x) (values (> x 1) :extra))

(deftest every-notany-notevery-one-value
  (list (multiple-value-list (every #'%spv-q (list 2 1)))
        (multiple-value-list (every #'%spv-q (vector 2 3)))
        (multiple-value-list (every (lambda (a b) (values (> a b) :extra)) (list 2 1) (list 1 5)))
        (multiple-value-list (notany #'%spv-q (list 1 1)))
        (multiple-value-list (notany #'%spv-q (vector 1 2)))
        (multiple-value-list (notevery #'%spv-q (list 2 1)))
        (multiple-value-list (notevery #'%spv-q (list 2 2)))
        (multiple-value-list (funcall 'every #'%spv-q (list 1))))
  ((nil) (t) (nil) (t) (nil) (t) (nil) (nil)))
