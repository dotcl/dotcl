;;; Regression: PARSE-INTEGER with :JUNK-ALLOWED T skipped the whitespace after
;;; the digits before returning the index, so the index pointed one past the
;;; separator. cl-pdf reads AFM lines by calling PARSE-INTEGER again from the
;;; returned index and checking the character there; it failed to load.
;;; Without :JUNK-ALLOWED, trailing whitespace is still skipped (index = end).

(deftest parse-integer-junk-allowed-index-1
  (multiple-value-list (parse-integer "-113 x" :junk-allowed t))
  (-113 4))

(deftest parse-integer-junk-allowed-index-start
  (multiple-value-list (parse-integer "FontBBox -113 -250 749 801" :start 8 :junk-allowed t))
  (-113 13))

(deftest parse-integer-junk-allowed-index-trailing-space
  (multiple-value-list (parse-integer "12  " :junk-allowed t))
  (12 2))

(deftest parse-integer-junk-allowed-index-radix
  (multiple-value-list (parse-integer "ff zz" :radix 16 :junk-allowed t))
  (255 2))

(deftest parse-integer-no-junk-trailing-space
  (multiple-value-list (parse-integer " -113  "))
  (-113 7))

(deftest parse-integer-junk-allowed-no-digits
  (list (multiple-value-list (parse-integer "-" :junk-allowed t))
        (multiple-value-list (parse-integer "  " :junk-allowed t)))
  ((nil 1) (nil 2)))
