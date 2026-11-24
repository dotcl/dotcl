;;; CHAR-EQUAL and CHAR-NOT-EQUAL of two operands in a test position are compiled as one bool call
;;; (no T / NIL object). The answer is the one the generic entry gives, for every character and
;;; for the errors: each character against ten representative ones, and a million pairs.

(defun %cet-eq (a b) (if (char-equal a b) t nil))
(defun %cet-ne (a b) (if (char-not-equal a b) t nil))
(defun %cet-generic-eq (a b) (funcall 'char-equal a b))
(defun %cet-generic-ne (a b) (funcall 'char-not-equal a b))

(defparameter *cet-partners*
  (mapcar #'code-char '(0 65 97 90 122 127 169 223 255 305 383 8490 54000 65535)))

(defun %cet-mismatches-for-char (c)
  (let ((n 0))
    (dolist (d *cet-partners* n)
      (unless (and (eq (%cet-eq c d) (and (%cet-generic-eq c d) t))
                   (eq (%cet-eq d c) (and (%cet-generic-eq d c) t))
                   (eq (%cet-ne c d) (and (%cet-generic-ne c d) t)))
        (incf n)))))

(deftest-compiled-only char-equal-test-position.every-character
  (let ((bad 0))
    (dotimes (code 65536 bad)
      (incf bad (%cet-mismatches-for-char (code-char code)))))
  0)

(deftest-compiled-only char-equal-test-position.random-pairs
  (let ((bad 0) (x 12345))
    (declare (fixnum bad x))
    (dotimes (i 1000000 bad)
      (setq x (logand (+ (* x 1103515245) 12345) #x7fffffff))
      (let* ((a (code-char (logand (ash x -3) #xffff)))
             (b (if (zerop (logand x 3))
                    (char-downcase a)
                    (if (= (logand x 3) 1)
                        (char-upcase a)
                        (code-char (logand (ash x -11) #xffff))))))
        (unless (and (eq (%cet-eq a b) (and (%cet-generic-eq a b) t))
                     (eq (%cet-ne a b) (and (%cet-generic-ne a b) t)))
          (incf bad)))))
  0)

(defun %cet-error-p (thunk)
  (handler-case (progn (funcall thunk) nil) (type-error () t)))

(deftest char-equal-test-position.errors-and-arity
  (list (%cet-error-p (lambda () (%cet-eq 1 #\a)))
        (%cet-error-p (lambda () (%cet-eq #\a "a")))
        (%cet-error-p (lambda () (%cet-ne nil #\a)))
        (%cet-error-p (lambda () (%cet-generic-eq 1 #\a)))
        (if (char-equal #\a #\A #\a) t nil)
        (if (char-equal #\a #\A #\b) t nil)
        (if (char-not-equal #\a #\b #\c) t nil)
        (if (char-not-equal #\a #\b #\A) t nil)
        (%cet-eq #\a #\A) (%cet-eq #\a #\b) (%cet-ne #\a #\A) (%cet-ne #\a #\b)
        (%cet-eq (code-char #xe9) (code-char #xc9))
        (%cet-eq (code-char #xd800) (code-char #xd800)))
  (t t t t t nil t nil t nil nil t t t))
