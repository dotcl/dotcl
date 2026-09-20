;;; A local declared CHARACTER can hold the character's code rather than a
;;; character object.
;;;
;;; dotcl characters are UTF-16 code units and CHAR-CODE-LIMIT is 65536, so the
;;; code is all a slot needs. Comparing two characters is comparing their codes,
;;; and a scanner compares every character it reads -- three times over, for a
;;; whitespace test -- so the object the slot used to hold was unwrapped once per
;;; comparison and never used for anything else.
;;;
;;; The representation is invisible from Lisp, which is what these tests check:
;;; the value is a character wherever a character is what the program asked for,
;;; the slot survives being handed to generic code, and the shapes that cannot
;;; take a raw slot -- a captured variable, an undeclared one, an init whose
;;; value may not be a character -- still answer the same.

(defparameter *nc-text* "ab cd")

;;; --- declared, used only in comparisons: the shape the representation is for

(defun nc-space-p (c)
  (declare (character c) (optimize (speed 3) (safety 0) (debug 0)))
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline)))

(defun nc-count-spaces (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n 0))
    (declare (fixnum n))
    (dotimes (i (length s) n)
      (declare (fixnum i))
      (let ((c (schar s i)))
        (declare (character c))
        (when (nc-space-p c) (setq n (the fixnum (1+ n))))))))

(deftest native-char-comparison-only
  (list (nc-count-spaces *nc-text*)
        (nc-count-spaces "")
        (nc-count-spaces "   ")
        (nc-count-spaces (coerce (list #\a #\Tab #\b #\Newline) 'string)))
  (1 0 3 2))

;;; --- the value is a character, not a code dressed up as one ---

(defun nc-identity (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((c (schar s i)))
    (declare (character c))
    c))

(deftest native-char-value-is-a-character
  (let ((c (nc-identity *nc-text* 1)))
    (list c (characterp c) (typep c 'character) (eql c #\b) (char-code c)))
  (#\b t t t 98))

;;; Handed to generic code: a list, a function argument, a return value.

(defun nc-generic-uses (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((c (schar s i)))
    (declare (character c))
    (list c (char-upcase c) (string c) (char-code c) (position c "abcd"))))

(deftest native-char-generic-positions
  (nc-generic-uses *nc-text* 3)
  (#\c #\C "c" 99 2))

;;; A literal init takes the same slot.

(defun nc-literal-init (x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((c #\q))
    (declare (character c))
    (list c (char= c x) (char-code c))))

(deftest native-char-literal-init
  (list (nc-literal-init #\q) (nc-literal-init #\z))
  ((#\q t 113) (#\q nil 113)))

;;; --- assignment into the slot ---

(defun nc-setq-walk (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((c #\?)
        (acc 0))
    (declare (character c) (fixnum acc))
    (dotimes (i (length s) (list c acc))
      (declare (fixnum i))
      (setq c (schar s i))
      (setq acc (the fixnum (+ acc (char-code c)))))))

(deftest native-char-setq-from-string
  (nc-setq-walk "abc")
  (#\c 294))

;;; Assigned a value that is not statically a character: the declaration still
;;; holds, and the slot still reads back as a character.

(defun nc-setq-computed (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((c #\a))
    (declare (character c))
    (setq c (code-char (+ (char-code #\a) n)))
    (list c (characterp c))))

(deftest native-char-setq-computed
  (list (nc-setq-computed 0) (nc-setq-computed 2))
  ((#\a t) (#\c t)))

;;; --- shapes that must NOT take a raw slot ---
;;;
;;; A captured variable keeps the boxed slot, because env capture stores an
;;; object. The closure and the body have to agree on the value.

(defun nc-captured (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((c (schar s i)))
    (declare (character c))
    (let ((peek (lambda () c)))
      (list c (funcall peek) (char= c (funcall peek))))))

(deftest native-char-captured-stays-boxed
  (nc-captured *nc-text* 0)
  (#\a #\a t))

;;; A CODE-CHAR init may answer NIL -- CLHS lets it, for a code at or above
;;; CHAR-CODE-LIMIT -- and NIL is not a value a code slot can hold. The binding
;;; must decline the raw slot, and the declaration is then simply the program's
;;; promise as before.

(defun nc-code-char-init (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((c (code-char n)))
    (declare (character c))
    (list c (characterp c))))

(deftest native-char-code-char-init
  (list (nc-code-char-init 97) (nc-code-char-init 65))
  ((#\a t) (#\A t)))

;;; No declaration: nothing changes.

(defun nc-undeclared (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((c (schar s i)))
    (list c (characterp c) (char= c (schar s i)))))

(deftest native-char-undeclared
  (nc-undeclared *nc-text* 4)
  (#\d t t))

;;; A special variable is bound on the dynamic stack, which has no slot to make
;;; native.

(defvar *nc-ch* #\z)

(defun nc-special-binding (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((*nc-ch* (schar s i)))
    (declare (character *nc-ch*))
    (list *nc-ch* (characterp *nc-ch*))))

(deftest native-char-special-binding
  (list (nc-special-binding *nc-text* 0) *nc-ch*)
  ((#\a t) #\z))

;;; --- the comparison answers agree with the generic ones ---

(defun nc-order (a b)
  (declare (character a b) (optimize (speed 3) (safety 0) (debug 0)))
  (list (char= a b) (char/= a b) (char< a b) (char> a b) (char<= a b) (char>= a b)))

(deftest native-char-full-comparison-set
  (list (nc-order #\a #\a) (nc-order #\a #\b) (nc-order #\b #\a))
  ((t nil nil nil t t) (nil t t nil t nil) (nil t nil t nil t)))

;;; Non-ASCII stays exact: LispChar interns only the ASCII range, so a code
;;; above it has to round-trip through the slot unchanged.

(defun nc-wide (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((c (code-char n)))
    (declare (character c))
    (let ((d c))
      (declare (character d))
      (list (char-code d) (char= c d)))))

(deftest native-char-non-ascii-round-trip
  (list (nc-wide 955) (nc-wide 65535) (nc-wide 12354))
  ((955 t) (65535 t) (12354 t)))
