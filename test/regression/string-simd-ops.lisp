;;; COUNT of a character in a string and one-argument STRING-UPCASE / STRING-DOWNCASE
;;; of an ASCII string take the vectorized paths; other strings take the scalar ones.

(defun %sso-text (n)
  (let ((s (make-string n :initial-element #\a)))
    (dotimes (i n) (setf (char s i) (code-char (+ 97 (mod i 26)))))
    s))

(defvar *sso-uni* (coerce (list #\a (code-char 233) #\B (code-char 223) #\e (code-char 960) #\E) 'string))

(deftest string-simd-ops.values
  (let ((s (%sso-text 100)) (u *sso-uni*))
    (list (count #\e s) (count #\e s :start 5) (count #\e s :end 5) (count #\e s :start 4 :end 5)
          (count #\E s) (count #\E s :test #'char-equal) (count #\e s :from-end t)
          (count #\e "") (count (code-char 233) u) (count #\e u) (count #\a u :test #'char-equal)
          (count #\z s :start 100)
          (string-upcase (subseq s 0 30)) (string-downcase (string-upcase (subseq s 0 30)))
          (string-upcase "") (string-upcase "abc-123_XyZ")
          (map 'list #'char-code (string-upcase u))
          (map 'list #'char-code (string-downcase u))
          (string-upcase #\a) (string-upcase 'foo) (string-downcase "Hello, World!")))
  (4 3 1 1 0 4 4 0 1 1 1 0 "ABCDEFGHIJKLMNOPQRSTUVWXYZABCD"
   "abcdefghijklmnopqrstuvwxyzabcd" "" "ABC-123_XYZ" (65 201 66 223 69 928 69)
   (97 233 98 223 101 960 101) "A" "FOO" "hello, world!"))
