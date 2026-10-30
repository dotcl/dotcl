;;; PPRINT-TAB after a deferred conditional newline uses the column that line
;;; will really have. Expected value is SBCL 2.6.8's output.

(deftest pprint-tab-after-deferred-newline.fill
  (let ((*print-right-margin* 16) (*print-pretty* t) (*print-miser-width* nil))
    (with-output-to-string (s)
      (pprint-logical-block (s '(a bb ccc dddd eeeee) :prefix "(" :suffix ")")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream s)
              (pprint-tab :line 8 0 s)
              (pprint-newline :fill s)))))
  #.(format nil "(A      BBCCC~% DDDD   EEEEE)"))
