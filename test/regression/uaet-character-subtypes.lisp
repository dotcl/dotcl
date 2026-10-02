;;; A compound type specifier that only admits characters, such as
;;; (MEMBER #\0 #\1) or (EQL #\a), is a subtype of CHARACTER, so an array
;;; specialized on it has to be a string (CLHS 15.1.2.1: upgrading preserves
;;; subtype relations). It upgraded to T, so MAKE-ARRAY built a general vector
;;; and PARSE-INTEGER rejected it as "not a string". Found by ansi-test's random
;;; type propagation tests (RANDOM-TYPE-PROP.PARSE-INTEGER.1).

(deftest uaet-character-subtypes
  (list (upgraded-array-element-type '(member #\0 #\1 #\2))
        (upgraded-array-element-type '(eql #\a))
        (upgraded-array-element-type '(or (eql #\a) standard-char))
        (upgraded-array-element-type '(and character (satisfies identity)))
        (let ((v (make-array 3 :element-type '(member #\0 #\1 #\2) :initial-element #\1)))
          (list (stringp v) (parse-integer v)))
        (upgraded-array-element-type '(member #\a 1)))
  (character character character character (t 111) t))
