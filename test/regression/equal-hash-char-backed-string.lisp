;;; An EQUAL hash table hashed a string written with (SETF CHAR) (char-array
;;; backed) by building a System.String of it on every lookup, and EQUAL of
;;; two such strings built two. The answers must not change.

;; A string built with (SETF CHAR) as an EQUAL key, found by an equal string
;; built the other way.
(deftest equal-hash-char-backed-string
  (let ((h (make-hash-table :test 'equal))
        (s (make-string 3)))
    (setf (char s 0) #\a (char s 1) #\b (char s 2) #\c)
    (setf (gethash s h) 1)
    (list (gethash "abc" h) (gethash (copy-seq "abc") h) (equal s "abc") (equal s "abd")
          (gethash (subseq "xabc" 1) h)))
  (1 1 t nil 1))

(deftest make-string-one-argument
  (list (length (make-string 4)) (make-string 2 :initial-element #\z) (length (make-string 0)))
  (4 "zz" 0))
