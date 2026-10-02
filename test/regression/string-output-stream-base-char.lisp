;;; A string output stream made with :ELEMENT-TYPE BASE-CHAR (or a subtype that
;;; upgrades to it) hands back a base string: ARRAY-ELEMENT-TYPE of the result
;;; has to agree with UPGRADED-ARRAY-ELEMENT-TYPE of the requested type.
;;; cl-who's test suite checks exactly this through WITH-OUTPUT-TO-STRING.

(deftest sos-base-char.with-output-to-string
  (let ((s (with-output-to-string (out nil :element-type 'base-char)
             (write-string "<br />" out))))
    (list s (eq (array-element-type s) (upgraded-array-element-type 'base-char))
          (typep s 'base-string)))
  ("<br />" t t))

(deftest sos-base-char.make-string-output-stream
  (let ((out (make-string-output-stream :element-type 'base-char)))
    (write-string "ab" out)
    (let ((s (get-output-stream-string out)))
      (list s (eq (array-element-type s) (upgraded-array-element-type 'base-char)))))
  ("ab" t))

(deftest sos-base-char.standard-char
  (let ((s (with-output-to-string (out nil :element-type 'standard-char)
             (write-char #\x out))))
    (eq (array-element-type s) (upgraded-array-element-type 'standard-char)))
  t)

(deftest sos-base-char.character-unchanged
  (array-element-type (with-output-to-string (out nil :element-type 'character)
                        (write-char #\x out)))
  character)

(deftest sos-base-char.reset-between-calls
  (let ((out (make-string-output-stream :element-type 'base-char)))
    (write-string "one" out)
    (let ((a (get-output-stream-string out)))
      (write-string "two" out)
      (list a (get-output-stream-string out))))
  ("one" "two"))
