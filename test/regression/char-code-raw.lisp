;;; (CHAR-CODE X) is a fixnum in [0, CHAR-CODE-LIMIT) whatever X is, so a sum of codes into a
;;; declared fixnum stays raw. An X that is no character signals the type-error CHAR-CODE does.

(defun %ccr-sum (n)
  (declare (fixnum n))
  (let ((sum 0))
    (declare (fixnum sum))
    (dotimes (i n)
      (let ((c (code-char (+ 33 (mod i 90)))))
        (incf sum (char-code (char-upcase c)))
        (incf sum (char-code c))
        (incf sum (char-code (code-char (char-code c))))
        (when (digit-char-p c) (incf sum))))
    sum))

(defun %ccr-sum-generic (n)
  (let ((sum 0))
    (dotimes (i n)
      (let ((c (funcall #'code-char (+ 33 (mod i 90)))))
        (incf sum (funcall #'char-code (funcall #'char-upcase c)))
        (incf sum (funcall #'char-code c))
        (incf sum (funcall #'char-code (funcall #'code-char (funcall #'char-code c))))
        (when (funcall #'digit-char-p c) (incf sum))))
    sum))

(defun %ccr-code (x) (declare (fixnum)) (+ 1 (char-code x)))
(defun %ccr-max () (+ (char-code (code-char (1- char-code-limit))) (char-code #\Nul)))

(deftest char-code-raw.same-as-generic
  (list (= (%ccr-sum 0) (%ccr-sum-generic 0))
        (= (%ccr-sum 1) (%ccr-sum-generic 1))
        (= (%ccr-sum 1000) (%ccr-sum-generic 1000))
        (%ccr-code #\A) (%ccr-code #\Nul) (%ccr-max) (%ccr-code (code-char 65535)))
  (t t t 66 1 65535 65536))

(deftest char-code-raw.not-a-character
  (list (handler-case (%ccr-code 5) (error (e) (type-of e)))
        (handler-case (%ccr-code nil) (error (e) (type-of e)))
        (handler-case (%ccr-code "a") (error (e) (type-of e)))
        (handler-case (%ccr-code 'a) (error (e) (type-of e))))
  (type-error type-error type-error type-error))

(deftest-compiled-only char-code-raw.no-allocation
  (progn (%ccr-sum 1000)
         (list (< (min-allocated-bytes (lambda () (%ccr-sum 200000))) 20000)))
  (t))
