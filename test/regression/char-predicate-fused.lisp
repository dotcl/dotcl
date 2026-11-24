;;; ALPHA-CHAR-P, UPPER-CASE-P, LOWER-CASE-P and ALPHANUMERICP in a test position (IF, WHEN,
;;; AND, COND, NOT) are one bool-returning call; in a value position and through FUNCALL they
;;; are the functions they were. Both must answer alike for every character, and for a
;;; value that is not a character.

(defun %cpf-test-alpha (c) (if (alpha-char-p c) t nil))
(defun %cpf-test-upper (c) (if (upper-case-p c) t nil))
(defun %cpf-test-lower (c) (when (lower-case-p c) t))
(defun %cpf-test-alnum (c) (cond ((alphanumericp c) t) (t nil)))
(defun %cpf-not-alpha (c) (if (not (alpha-char-p c)) :no :yes))
(defun %cpf-and (c) (and (alpha-char-p c) (not (upper-case-p c)) :lower-letter))
(defun %cpf-value (c) (list (alpha-char-p c) (upper-case-p c) (lower-case-p c) (alphanumericp c)))
(defun %cpf-via-funcall (c)
  (list (funcall #'alpha-char-p c) (funcall #'upper-case-p c)
        (funcall #'lower-case-p c) (funcall #'alphanumericp c)))
(defun %cpf-via-test (c)
  (list (%cpf-test-alpha c) (%cpf-test-upper c) (%cpf-test-lower c) (%cpf-test-alnum c)))

(deftest char-predicate-fused.every-bmp-character
  (let ((bad 0))
    (dotimes (code #x10000)
      (let* ((c (code-char code)))
        (when c
          (let ((v (%cpf-via-funcall c)) (tst (%cpf-via-test c)) (val (%cpf-value c)))
            (unless (and (equal val v)
                         (equal (mapcar (lambda (x) (and x t)) v) tst)
                         (eq (and (alpha-char-p c) (not (upper-case-p c)) :lower-letter) (%cpf-and c))
                         (eq (if (alpha-char-p c) :yes :no) (%cpf-not-alpha c)))
              (incf bad))))))
    bad)
  0)

(deftest char-predicate-fused.known-characters
  (list (%cpf-via-test #\a) (%cpf-via-test #\A) (%cpf-via-test #\5) (%cpf-via-test #\Space)
        (%cpf-via-test (code-char #xE9)) (%cpf-via-test (code-char #xC9))
        (%cpf-via-test (code-char #x3B1)) (%cpf-via-test (code-char #x4E2D))
        (%cpf-via-test (code-char 0)) (%cpf-via-test (code-char #xD800))
        (%cpf-via-test (code-char #x1C5)))
  ((t nil t t) (t t nil t) (nil nil nil t) (nil nil nil nil)
   (t nil t t) (t t nil t) (t nil t t) (t nil nil t)
   (nil nil nil nil) (nil nil nil nil) (t nil nil t)))

(deftest char-predicate-fused.not-a-character
  (flet ((e (f x) (handler-case (funcall f x) (type-error () :type-error))))
    (list (e #'%cpf-test-alpha 65) (e #'%cpf-test-upper "a") (e #'%cpf-test-lower nil)
          (e #'%cpf-test-alnum :a) (e #'%cpf-value 1.5) (e #'%cpf-not-alpha '(1))
          (e #'%cpf-and 3)))
  (:type-error :type-error :type-error :type-error :type-error :type-error :type-error))

(deftest char-predicate-fused.in-a-string-loop
  (let ((s "Hello, World 42 ünï!") (a 0) (u 0) (l 0) (n 0))
    (dotimes (i (length s))
      (let ((c (char s i)))
        (when (alpha-char-p c) (incf a))
        (when (upper-case-p c) (incf u))
        (when (lower-case-p c) (incf l))
        (when (alphanumericp (char s i)) (incf n))))
    (list a u l n (count-if #'alpha-char-p s) (remove-if-not #'upper-case-p s)))
  (13 2 11 15 13 "HW"))

(deftest char-predicate-fused.local-function-shadows
  (flet ((alpha-char-p (x) (list :mine x)))
    (declare (notinline alpha-char-p))
    (if (alpha-char-p 5) :truthy :falsy))
  :truthy)
