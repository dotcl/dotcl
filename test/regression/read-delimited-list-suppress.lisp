;;; READ-DELIMITED-LIST returns NIL while *READ-SUPPRESS* is true (it used to return a list
;;; of NILs, one per skipped element); with it false it returns the elements as before.

(deftest read-delimited-list-suppress.suppressed
  (let ((*read-suppress* t))
    (with-input-from-string (s "1 2 3) rest")
      (list (read-delimited-list #\) s) (read-char s))))
  (nil #\Space))

(deftest read-delimited-list-suppress.not-suppressed
  (with-input-from-string (s "1 2 3)")
    (read-delimited-list #\) s))
  (1 2 3))

(deftest read-delimited-list-suppress.empty
  (list (let ((*read-suppress* t)) (with-input-from-string (s ")") (read-delimited-list #\) s)))
        (with-input-from-string (s ")") (read-delimited-list #\) s)))
  (nil nil))

(deftest read-delimited-list-suppress.in-skipped-form
  (let ((*read-suppress* t))
    (with-input-from-string (s "(a (b c) d) 7")
      (list (read s) (let ((*read-suppress* nil)) (read s)))))
  (nil 7))
