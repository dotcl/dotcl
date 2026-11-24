;;; A PPRINT-LOGICAL-BLOCK that opens on a mandatory newline leaves its first line empty:
;;; the blanks of a per-line prefix written before the newline are trimmed, as the blanks
;;; before any newline of the layout are.

(defun pbm-print (thunk)
  (let ((*print-pretty* t) (*print-right-margin* 40))
    (with-output-to-string (s) (funcall thunk s))))

(deftest pprint-prefix-before-mandatory.block-first
  (pbm-print (lambda (s)
               (pprint-logical-block (s '("a") :per-line-prefix "  ")
                 (pprint-newline :mandatory s)
                 (write (pprint-pop) :stream s))))
  "
  \"a\"")

(deftest pprint-prefix-before-mandatory.after-first-item
  (pbm-print (lambda (s)
               (pprint-logical-block (s '("a" "b") :per-line-prefix ";; ")
                 (write (pprint-pop) :stream s)
                 (pprint-newline :mandatory s)
                 (write (pprint-pop) :stream s))))
  ";; \"a\"
;; \"b\"")

(deftest pprint-prefix-before-mandatory.plain-prefix-kept
  (pbm-print (lambda (s)
               (pprint-logical-block (s '("a") :prefix "(" :suffix ")")
                 (pprint-newline :mandatory s)
                 (write (pprint-pop) :stream s))))
  "(
 \"a\")")

;; The blanks the caller wrote before a block are the caller's own: only the blanks that end the
;; block's own prefix go before a mandatory newline.
(deftest pprint-prefix-before-mandatory.callers-blanks-stay
  (flet ((out (lead kw prefix)
           (with-output-to-string (s)
             (write-string lead s)
             (if (eq kw :per-line)
                 (pprint-logical-block (s nil :per-line-prefix prefix)
                   (pprint-newline :mandatory s) (write-string "a" s))
                 (pprint-logical-block (s nil :prefix prefix)
                   (pprint-newline :mandatory s) (write-string "a" s))))))
    (list (substitute #\| #\Newline (out "ab " :per-line ""))
          (substitute #\| #\Newline (out "ab  " :prefix ""))
          (substitute #\| #\Newline (out "   " :per-line ""))))
  ("ab |   a" "ab  |    a" "   |   a"))
