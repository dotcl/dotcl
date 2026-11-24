;;; (SET-MACRO-CHARACTER char NIL) makes the character an ordinary constituent, as SBCL does;
;;; it is what restoring the answer of GET-MACRO-CHARACTER for a non-macro character does.

(deftest set-macro-character-nil.constituent
  (let ((*readtable* (copy-readtable)))
    (list (set-macro-character #\] nil)
          (multiple-value-list (get-macro-character #\]))
          (read-from-string "ab]cd")))
  (t (nil nil) ab]cd))

(deftest set-macro-character-nil.removes-a-reader-macro
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\$ (lambda (s c) (declare (ignore s c)) :dollar))
    (let ((with (read-from-string "$")))
      (set-macro-character #\$ nil)
      (list with (read-from-string "$x") (get-macro-character #\$))))
  (:dollar $x nil))

(deftest set-macro-character-nil.terminating-character
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\) nil)
    (values (read-from-string "a)b")))
  a\)b)

(deftest set-macro-character-nil.restores-a-saved-answer
  ;; The usual idiom: put back what GET-MACRO-CHARACTER said, macro or not.
  (let ((*readtable* (copy-readtable)))
    (let ((saved-a (multiple-value-list (get-macro-character #\a)))
          (saved-paren (multiple-value-list (get-macro-character #\())))
      (set-macro-character #\a (lambda (s c) (declare (ignore s c)) :a-macro))
      (set-macro-character #\( (lambda (s c) (declare (ignore s c)) :paren-macro))
      (let ((during (list (read-from-string "a") (read-from-string "(x)"))))
        (set-macro-character #\a (first saved-a) (second saved-a))
        (set-macro-character #\( (first saved-paren) (second saved-paren))
        (list during (read-from-string "abc") (read-from-string "(x)")))))
  ((:a-macro :paren-macro) abc (x)))

(deftest set-macro-character-nil.standard-readtable-untouched
  (list (read-from-string "(a b)") (get-macro-character #\)))
  ((a b) #.(get-macro-character #\))))

(deftest set-macro-character-nil.other-errors-stay
  (list (handler-case (set-macro-character #\x 5) (error () :error))
        (handler-case (set-macro-character "x" nil) (error () :error)))
  (:error :error))
