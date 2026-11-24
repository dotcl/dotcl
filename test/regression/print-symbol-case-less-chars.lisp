;;; A symbol name is written with |...| only when the reader would change it.
;;; Characters the reader leaves alone in the :upcase readtable (sharp s, dotless
;;; i) need no bars; characters it does upcase (micro sign, long s, e acute)
;;; do. Every name prints so that READ gives the same symbol back.
;;; Expected values are what SBCL returns.

(defun %pscl-name (code) (string (code-char code)))

(deftest print-symbol-case-less-chars.no-bars
  (list (prin1-to-string (intern (%pscl-name 223)))
        (prin1-to-string (intern (%pscl-name 305))))
  (#.(string (code-char 223)) #.(string (code-char 305))))

(deftest print-symbol-case-less-chars.bars
  (list (prin1-to-string (intern (%pscl-name 181)))
        (prin1-to-string (intern (%pscl-name 383)))
        (prin1-to-string (intern (%pscl-name 233))))
  (#.(format nil "|~A|" (code-char 181)) #.(format nil "|~A|" (code-char 383))
   #.(format nil "|~A|" (code-char 233))))

(deftest print-symbol-case-less-chars.round-trip
  (loop for code in '(181 383 223 305 233 255 937 453)
        always (let ((s (intern (%pscl-name code))))
                 (eq s (read-from-string (prin1-to-string s)))))
  t)
