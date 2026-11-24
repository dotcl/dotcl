;;; REVERSE of a string writes the reversed characters into one new string.

(deftest reverse-string-one-copy.basic
  (list (reverse "") (reverse "a") (reverse "abc") (reverse "hello world")
        (string= (reverse (reverse "a longer string")) "a longer string"))
  ("" "a" "cba" "dlrow olleh" t))

(deftest reverse-string-one-copy.source-unchanged-and-writable
  (let* ((s (copy-seq "abcd")) (r (reverse s)))
    (setf (char r 0) #\X)
    (list s r (eq s r)))
  ("abcd" "Xcba" nil))

(deftest reverse-string-one-copy.after-mutation
  (let ((s (copy-seq "abcd")))
    (setf (char s 1) #\Z)
    (list (reverse s) (reverse (nreverse (copy-seq "xy")))))
  ("dcZa" "xy"))
