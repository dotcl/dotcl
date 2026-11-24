;;; CHAR=, CHAR/= and (EQL x #\c) in a test position compare codes in one bool call when the
;;; operands' codes are not known statically (an untyped parameter, a call result). Same
;;; answers, same errors as the generic entries.

(defun %ccf-eq (c) (if (char= c #\a) 1 2))
(defun %ccf-ne (c) (if (char/= c #\a) 1 2))
(defun %ccf-eq2 (a b) (if (char= a b) 1 2))
(defun %ccf-eql (c) (if (eql c #\a) 1 2))
(defun %ccf-eql-r (c) (if (eq #\a c) 1 2))
(defun %ccf-case (c) (case c (#\{ 1) (#\[ 2) (#\" 3) (#\t 4) (#\f 5) ((#\x #\y) 6) (t 7)))
(defun %ccf-mv () (values #\a #\b))
(defun %ccf-wild (c) (and (char/= c #\Space) (not (char= c #\Tab)) (or (eql c #\z) :other)))

(deftest char-compare-fused.chars
  (list (%ccf-eq #\a) (%ccf-eq #\b) (%ccf-ne #\a) (%ccf-ne #\b) (%ccf-eq2 #\q #\q) (%ccf-eq2 #\q #\r)
        (%ccf-eql #\a) (%ccf-eql #\A) (%ccf-eql-r #\a) (%ccf-eql-r #\b)
        (mapcar #'%ccf-case (list #\{ #\[ #\" #\t #\f #\x #\y #\z #\a))
        (%ccf-wild #\Space) (%ccf-wild #\Tab) (%ccf-wild #\z) (%ccf-wild #\k))
  (1 2 2 1 1 2 1 2 1 2 (1 2 3 4 5 6 6 7 7) nil nil t :other))

(deftest char-compare-fused.bmp-edge-characters
  (let ((a (code-char #xFFFE)) (b (code-char #xFFFF)) (c (code-char #x3042)))
    (list (%ccf-eq2 a a) (%ccf-eq2 a b) (%ccf-eq2 a c) (%ccf-eql a) (%ccf-case a)
          (if (char/= a b) 1 2) (if (eql a (code-char #xFFFE)) 1 2)))
  (1 2 2 2 7 1 1))

(deftest char-compare-fused.non-characters
  (list (%ccf-eql 97) (%ccf-eql "a") (%ccf-eql nil) (%ccf-eql 'a) (%ccf-eql 97.0) (%ccf-eql-r :a)
        (mapcar #'%ccf-case (list 1 "x" nil :t))
        (handler-case (%ccf-eq 97) (type-error () :type-error))
        (handler-case (%ccf-ne "a") (type-error () :type-error))
        (handler-case (%ccf-eq2 #\a 1) (type-error () :type-error))
        (handler-case (%ccf-eq2 nil #\a) (type-error () :type-error)))
  (2 2 2 2 2 2 (7 7 7 7) :type-error :type-error :type-error :type-error))

(deftest char-compare-fused.multiple-values-and-evaluation
  (let ((log nil))
    (flet ((c (x) (push x log) x))
      (list (if (char= (%ccf-mv) #\a) 1 2) (if (char= #\b (%ccf-mv)) 1 2) (if (eql (%ccf-mv) #\a) 1 2)
            (if (char= (c #\a) (c #\a)) 1 2) (reverse log))))
  (1 2 1 1 (#\a #\a)))

(defun %ccf-quoted (c) (list (if (eql c '#\a) 1 2) (if (eq '#\b c) 1 2) (if (char= c '#\a) 1 2)))
(defun %ccf-code-edges (c) (list (if (eql c (code-char 0)) 1 2) (if (eql c #\Nul) 1 2) (if (char/= c #\Nul) 1 2)))

(deftest char-compare-fused.code-immediate
  (list (%ccf-quoted #\a) (%ccf-quoted #\b) (%ccf-quoted #\c)
        (%ccf-code-edges (code-char 0)) (%ccf-code-edges #\a)
        (handler-case (%ccf-quoted 97) (type-error () :type-error))
        (handler-case (%ccf-code-edges nil) (type-error () :type-error)))
  ((1 2 1) (2 1 2) (2 2 2) (1 1 2) (2 2 1) :type-error :type-error))
