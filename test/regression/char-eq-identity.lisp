;;; The same character is EQ to itself, beyond ASCII too.
;;;
;;; The standard leaves EQ on characters to the implementation, but the
;;; mainstream ones all answer true for the same character, and libraries rely
;;; on it: cl-ppcre's charset stores characters in a vector and looks them up
;;; with EQ, so every non-ASCII member of a character class was missed while
;;; (code-char 200) made a fresh object each call. Only ASCII was cached.

(deftest char-eq-identity.code-char
  (list (eq (code-char 65) (code-char 65))
        (eq (code-char 200) (code-char 200))
        (eq (code-char 1000) (code-char 1000))
        (eq (code-char 40000) (code-char 40000))
        (eq (code-char 65535) (code-char 65535)))
  (t t t t t))

(deftest char-eq-identity.from-string-and-reader
  (let ((s (coerce (list (code-char 233) (code-char 12354)) 'string)))
    (list (eq (char s 0) (code-char 233))
          (eq (char s 1) (code-char 12354))
          (eq (char (string-upcase s) 0) (char-upcase (code-char 233)))))
  (t t t))

;; What cl-ppcre does: store a character in a character vector, read it back,
;; compare with EQ.
(deftest char-eq-identity.through-a-character-vector
  (let ((v (make-array 4 :element-type 'character :initial-element (code-char 0))))
    (setf (char v 1) (code-char 1000))
    (list (eq (char v 1) (code-char 1000))
          (eq (char v 0) (code-char 0))))
  (t t))
