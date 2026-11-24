;;; Hash-table functions given a non-table, and FILL given a bad :START / :END, name the expected type.

(defmacro %teh (form)
  `(handler-case ,form
     (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))
     (error (e) (list :other (type-of e)))))

(deftest type-error-hash-fill.hash-table-functions
  (list (%teh (gethash 1 (list 1))) (%teh (remhash 1 5)) (%teh (clrhash 'a))
        (%teh (hash-table-count (list 1))) (%teh (hash-table-test 5)) (%teh (hash-table-size "s")))
  (((1) hash-table) (5 hash-table) (a hash-table) ((1) hash-table) (5 hash-table) ("s" hash-table)))

(deftest type-error-hash-fill.fill-start-end
  (list (%teh (fill (copy-seq "ab") 1 :start 'a)) (%teh (fill (copy-seq "ab") 1 :start -1))
        (%teh (fill (copy-seq "ab") 1 :end 1.5)))
  ((a unsigned-byte) (-1 unsigned-byte) (1.5 (or null unsigned-byte))))
