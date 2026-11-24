;;; The ordering comparisons of characters with two operands the compiler cannot read as codes, and
;;; the chained three-operand forms, in a test position: one bool call. Same answers and errors.

(defun %ccx-le3 (c) (if (char<= #\0 c #\9) 1 2))
(defun %ccx-lt3 (a b c) (if (char< a b c) 1 2))
(defun %ccx-gt3 (a b c) (if (char> a b c) 1 2))
(defun %ccx-ge3 (a b c) (if (char>= a b c) 1 2))
(defun %ccx-eq3 (a b c) (if (char= a b c) 1 2))
(defun %ccx-lt (a b) (if (char< a b) 1 2))
(defun %ccx-gt (a b) (if (char> a b) 1 2))
(defun %ccx-le (a b) (if (char<= a b) 1 2))
(defun %ccx-ge (a b) (if (char>= a b) 1 2))
(defun %ccx-and (c) (and (char<= #\a c #\z) (char/= c #\q) :lower))

(deftest char-chain-fused.three-operands
  (list (mapcar #'%ccx-le3 (list #\0 #\5 #\9 #\/ #\: #\a))
        (%ccx-lt3 #\a #\b #\c) (%ccx-lt3 #\a #\a #\c) (%ccx-lt3 #\c #\b #\a)
        (%ccx-gt3 #\c #\b #\a) (%ccx-gt3 #\c #\c #\a)
        (%ccx-ge3 #\c #\c #\a) (%ccx-ge3 #\c #\b #\b) (%ccx-ge3 #\a #\b #\c)
        (%ccx-eq3 #\a #\a #\a) (%ccx-eq3 #\a #\a #\b) (%ccx-eq3 #\a #\b #\a))
  ((1 1 1 2 2 2) 1 2 2 1 2 1 1 2 1 2 2))

(deftest char-chain-fused.two-operands
  (list (%ccx-lt #\a #\b) (%ccx-lt #\b #\b) (%ccx-gt #\b #\a) (%ccx-gt #\a #\a)
        (%ccx-le #\a #\a) (%ccx-le #\b #\a) (%ccx-ge #\b #\b) (%ccx-ge #\a #\b)
        (%ccx-lt (code-char 0) (code-char 65535)) (%ccx-ge (code-char 65535) (code-char 0)))
  (1 2 1 2 1 2 1 2 1 1))

(deftest char-chain-fused.non-characters-are-errors
  (list (handler-case (%ccx-le3 5) (type-error () :type-error))
        (handler-case (%ccx-lt3 #\a "b" #\c) (type-error () :type-error))
        (handler-case (%ccx-lt3 #\z #\a 7) (type-error () :type-error))
        (handler-case (%ccx-gt3 nil #\a #\b) (type-error () :type-error))
        (handler-case (%ccx-lt #\a 1) (type-error () :type-error))
        (handler-case (%ccx-ge "a" #\a) (type-error () :type-error))
        (handler-case (%ccx-eq3 #\a #\a :k) (type-error () :type-error)))
  (:type-error :type-error :type-error :type-error :type-error :type-error :type-error))

(deftest char-chain-fused.in-and-cond
  (list (mapcar #'%ccx-and (list #\a #\q #\z #\A #\0))
        (let ((n 0)) (dolist (c (coerce "a1b22c333" 'list) n) (when (char<= #\0 c #\9) (incf n))))
        (funcall (lambda (c) (char<= #\a c #\z)) #\m) (funcall (lambda (c) (char<= #\a c #\z)) #\M))
  ((:lower nil :lower nil nil) 6 t nil))

;;; The same comparisons as a value (the answer of a function), which is how CL-PPCRE's
;;; DIGIT-CHAR-P and its friends use them.
(defun %ccx-v3 (c) (char<= #\0 c #\9))
(defun %ccx-v3lt (a b c) (char< a b c))
(defun %ccx-v3eq (a b c) (char= a b c))
(defun %ccx-v2 (a b) (char< a b))
(defun %ccx-v2ge (a b) (char>= a b))
(defun %ccx-vmany (a b c d) (char< a b c d))
(defun %ccx-v1 (a) (char< a))

(deftest char-chain-fused.as-a-value
  (list (mapcar #'%ccx-v3 (list #\0 #\9 #\a #\/)) (%ccx-v3lt #\a #\b #\c) (%ccx-v3lt #\a #\c #\b)
        (%ccx-v3eq #\a #\a #\a) (%ccx-v2 #\a #\b) (%ccx-v2 #\b #\a) (%ccx-v2ge #\b #\b)
        (%ccx-vmany #\a #\b #\c #\d) (%ccx-vmany #\a #\b #\c #\c) (%ccx-v1 #\a)
        (handler-case (%ccx-v3 7) (type-error () :type-error))
        (handler-case (%ccx-v2 #\a "b") (type-error () :type-error))
        (handler-case (%ccx-v1 5) (type-error () :type-error))
        (funcall #'%ccx-v3 #\5) (funcall #'char<= #\a #\b #\c)
        (flet ((char< (a b) (declare (ignore a b)) :mine)) (char< #\z #\a)))
  ((t t nil nil) t nil t t nil t t nil t :type-error :type-error :type-error t t :mine))
