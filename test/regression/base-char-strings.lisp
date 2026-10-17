;;; BASE-CHAR is a character whose code is below 128, as in SBCL. A string is a
;;; BASE-STRING only when it was made with element type BASE-CHAR (or
;;; STANDARD-CHAR); a CHARACTER string is (SIMPLE-ARRAY CHARACTER (*)). TYPEP,
;;; TYPE-OF, SUBTYPEP, ARRAY-ELEMENT-TYPE and COERCE agree on which is which.

(deftest base-char-strings.chars
  (list (typep #\a 'base-char) (typep (code-char 127) 'base-char)
        (typep (code-char 128) 'base-char) (typep (code-char 200) 'extended-char)
        (typep #\a 'extended-char) (type-of (code-char #x3042))
        (subtypep 'character 'base-char) (subtypep 'base-char 'character))
  (t t nil t nil extended-char nil t))

(deftest base-char-strings.character-string-is-not-base
  (let ((s (make-array 2 :element-type 'character :initial-element #\a)))
    (list (typep s 'base-string) (typep s 'simple-base-string)
          (typep s '(simple-array character (*))) (typep s 'simple-string)
          (type-of s) (array-element-type s)
          (typep (make-string 2) 'base-string) (typep "abc" 'base-string)
          (typep (copy-seq "abc") 'base-string)))
  (nil nil t t (simple-array character (2)) character nil nil nil))

(deftest base-char-strings.base-string-made-explicitly
  (mapcar (lambda (s)
            (list (typep s 'simple-base-string) (typep s '(simple-array character (*)))
                  (array-element-type s) (type-of s) (typep s (type-of s)) (string= s "ab")))
          (list (make-string 2 :element-type 'base-char :initial-element #\a)
                (make-array 2 :element-type 'standard-char :initial-element #\a)
                (coerce "ab" 'base-string)
                (coerce '(#\a #\b) 'simple-base-string)
                (coerce "ab" '(vector base-char))
                (concatenate 'base-string "a" "b")
                (map 'base-string #'identity "ab")
                (make-sequence 'base-string 2 :initial-element #\a)))
  ((t nil base-char (simple-base-string 2) t nil)
   (t nil base-char (simple-base-string 2) t nil)
   (t nil base-char (simple-base-string 2) t t)
   (t nil base-char (simple-base-string 2) t t)
   (t nil base-char (simple-base-string 2) t t)
   (t nil base-char (simple-base-string 2) t t)
   (t nil base-char (simple-base-string 2) t t)
   (t nil base-char (simple-base-string 2) t nil)))

(deftest base-char-strings.coerce-extended-char-is-an-error
  (handler-case (coerce (string (code-char 200)) 'base-string)
    (type-error () :type-error))
  :type-error)

(deftest base-char-strings.subtypep
  (mapcar (lambda (p) (multiple-value-list (subtypep (first p) (second p))))
          '((base-string (vector base-char)) ((vector standard-char) base-string)
            ((vector character) base-string) (base-string (vector character))
            ((simple-array character (*)) simple-base-string) (base-string string)))
  ((t t) (t t) (nil t) (nil t) (nil t) (t t)))

(deftype %bcs-token-string () '(and (not (vector nil)) (not base-string) (array character 1)))

(deftest base-char-strings.not-base-string-deftype
  (list (typep "abc" '%bcs-token-string)
        (typep (make-array 3 :element-type 'character :adjustable t :fill-pointer 0)
               '%bcs-token-string)
        (typep (coerce "abc" 'base-string) '%bcs-token-string))
  (t t nil))
