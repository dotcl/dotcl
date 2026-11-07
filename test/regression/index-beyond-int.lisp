;;; An index or time beyond what this implementation can represent is a TYPE-ERROR,
;;; not a wrapped small value and not a raw .NET message.

(deftest index-beyond-int.elt-string
  (handler-case (elt "abc" 4294967296) (type-error (e) (type-error-datum e)))
  4294967296)

(deftest index-beyond-int.elt-list
  (handler-case (elt (list 1 2 3) 4294967296) (type-error () :type-error))
  :type-error)

(deftest index-beyond-int.elt-vector
  (handler-case (elt (vector 1 2 3) 2147483648) (type-error () :type-error))
  :type-error)

(deftest index-beyond-int.elt-ok
  (list (elt "abc" 2) (elt '(1 2 3) 1) (elt #(1 2 3) 0))
  (#\c 2 1))

(deftest index-beyond-int.decode-universal-time
  (handler-case (decode-universal-time most-positive-fixnum 0)
    (type-error (e) (and (search "years 1 to 9999" (princ-to-string e)) t)))
  t)

(deftest index-beyond-int.decode-universal-time-ok
  (multiple-value-list (decode-universal-time 0 0))
  (0 0 0 1 1 1900 0 nil 0))

(deftest index-beyond-int.subseq-start
  (handler-case (subseq "abc" 4294967296) (type-error () :type-error))
  :type-error)

(deftest index-beyond-int.subseq-end
  (handler-case (subseq "abc" 0 4294967296) (type-error () :type-error))
  :type-error)

(deftest index-beyond-int.search-start2
  (handler-case (search "a" "abc" :start2 4294967296) (type-error () :type-error))
  :type-error)

(deftest index-beyond-int.string-upcase-start
  (handler-case (string-upcase "abc" :start 4294967296) (type-error () :type-error))
  :type-error)

(deftest index-beyond-int.print-base
  (handler-case (write-to-string 5 :base 4294967298) (type-error () :type-error))
  :type-error)

(deftest index-beyond-int.read-from-string-start
  (handler-case (read-from-string "abc" t nil :start 4294967296) (type-error () :type-error))
  :type-error)

(deftest index-beyond-int.read-from-string-start-small
  (handler-case (read-from-string "abc" t nil :start 10) (type-error () :type-error))
  :type-error)

(deftest index-beyond-int.read-from-string-ok
  (multiple-value-list (read-from-string "x abc" t nil :start 2))
  (abc 5))

(deftest index-beyond-int.negative-and-boundary
  (list (handler-case (elt "abc" -1) (type-error () :type-error))
        (handler-case (subseq "abc" -1) (type-error () :type-error))
        (handler-case (elt "abc" 2147483647) (type-error () :type-error))
        (handler-case (elt "abc" 2147483648) (type-error () :type-error))
        (subseq "abc" 1 3))
  (:type-error :type-error :type-error :type-error "bc"))
