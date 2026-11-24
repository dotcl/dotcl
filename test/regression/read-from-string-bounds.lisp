;;; READ-FROM-STRING takes START as an index and END as an index or NIL; a
;;; non-integer bound was accepted and out-of-range bounds leaked host errors.

(defun %rfsb (thunk)
  (handler-case (funcall thunk) (type-error () :type-error)))

(deftest read-from-string-bounds.non-integer
  (list (%rfsb (lambda () (read-from-string "abc" t nil :start 1.5)))
        (%rfsb (lambda () (read-from-string "abc" t nil :end 1.5)))
        (%rfsb (lambda () (read-from-string "abc" t nil :end "2"))))
  (:type-error :type-error :type-error))

(deftest read-from-string-bounds.out-of-range
  (list (%rfsb (lambda () (read-from-string "abc" t nil :start -1)))
        (%rfsb (lambda () (read-from-string "abc" t nil :start 4)))
        (%rfsb (lambda () (read-from-string "abc" t nil :end 4)))
        (%rfsb (lambda () (read-from-string "abc" t nil :start 2 :end 1))))
  (:type-error :type-error :type-error :type-error))

(deftest read-from-string-bounds.valid
  (list (multiple-value-list (read-from-string "abcdef" t nil :start 1 :end 4))
        (multiple-value-list (read-from-string "abc" t nil :start 0 :end nil))
        (multiple-value-list (read-from-string "abc" nil nil :start 3 :end 3))
        (multiple-value-list (read-from-string "abc" t nil :start nil))
        (multiple-value-list (read-from-string "abc" t nil :start 1)))
  ((bcd 4) (abc 3) (nil 3) (abc 3) (bc 3)))
