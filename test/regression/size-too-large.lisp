;;; A size that is a valid integer but too large for a sequence or array is a
;;; TYPE-ERROR, not a silently wrapped small size.

(deftest size-too-large.make-list
  (handler-case (make-list (expt 10 12)) (type-error (e) (type-error-datum e)))
  1000000000000)

(deftest size-too-large.make-list.wrap
  (handler-case (make-list 4294967296) (type-error () :type-error))
  :type-error)

(deftest size-too-large.make-string
  (handler-case (make-string (expt 10 12)) (type-error () :type-error))
  :type-error)

(deftest size-too-large.make-array
  (handler-case (make-array (expt 10 12)) (type-error () :type-error))
  :type-error)

(deftest size-too-large.small-make-list
  (make-list 3 :initial-element 1)
  (1 1 1))
