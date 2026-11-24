;;; (SETF (CHAR S I) C) with a subscript the compiler knows is a fixnum, and (CODE-CHAR N) of a
;;; fixnum, take the subscript / code raw. The answers and the conditions are the generic ones.

(defun %csl-fill (n)
  (declare (fixnum n))
  (let ((s (make-string n)))
    (dotimes (i n) (setf (char s i) (code-char (+ 97 (mod i 26)))))
    s))

(defun %csl-fill-into (s n)
  (declare (fixnum n))
  (dotimes (i n) (setf (char s i) (code-char (+ 97 (mod i 26)))))
  s)

(defun %csl-set (s i c) (declare (fixnum i)) (setf (char s i) c))
(defun %csl-setf-value (s i) (declare (fixnum i)) (list (setf (char s i) #\z) (char s i)))
(defun %csl-code-char (n) (declare (fixnum n)) (code-char n))
(defun %csl-order (s)
  (declare (fixnum))
  (let ((log '()) (i 0))
    (declare (fixnum i))
    (setf (char (progn (push :str log) s) (progn (push :idx log) (incf i) i)) (progn (push :val log) #\q))
    (list (reverse log) s)))

(deftest char-set-long.fill
  (list (%csl-fill 0) (%csl-fill 1) (%csl-fill 30))
  ("" "a" "abcdefghijklmnopqrstuvwxyzabcd"))

(deftest char-set-long.result-and-bounds
  (let ((s (make-string 3 :initial-element #\a)))
    (list (%csl-setf-value s 0) (%csl-setf-value s 2) s
          (handler-case (%csl-set s 3 #\b) (error (e) (type-of e)))
          (handler-case (%csl-set s -1 #\b) (error (e) (type-of e)))
          (handler-case (%csl-set s most-positive-fixnum #\b) (error (e) (type-of e)))
          (handler-case (%csl-set s 0 65) (error (e) (type-of e)))
          (handler-case (%csl-set s 0 nil) (error (e) (type-of e)))
          (handler-case (%csl-set 5 0 #\b) (error (e) (type-of e)))
          (handler-case (%csl-set nil 0 #\b) (error (e) (type-of e)))))
  ((#\z #\z) (#\z #\z) "zaz" type-error type-error type-error type-error type-error type-error type-error))

(deftest char-set-long.string-kinds
  (let ((adj (make-array 4 :element-type 'character :adjustable t :initial-element #\a))
        (fp (make-array 4 :element-type 'character :fill-pointer 2 :initial-element #\a))
        (base (make-string 2 :element-type 'base-char :initial-element #\a))
        (disp (make-array 2 :element-type 'character :displaced-to (make-string 5 :initial-element #\a)
                                                      :displaced-index-offset 1)))
    (%csl-set adj 1 #\b) (%csl-set fp 1 #\c) (%csl-set base 0 #\d) (%csl-set disp 0 #\e)
    (list (coerce adj 'list) (coerce fp 'list) (coerce base 'list) (coerce disp 'list)
          (handler-case (%csl-set fp 3 #\x) (error (e) (type-of e)))))
  ((#\a #\b #\a #\a) (#\a #\c) (#\d #\a) (#\e #\a) #\x))

(deftest char-set-long.code-char
  (list (%csl-code-char 0) (%csl-code-char 65) (%csl-code-char 255) (%csl-code-char 256)
        (%csl-code-char (1- char-code-limit))
        (%csl-code-char char-code-limit)
        (%csl-code-char most-positive-fixnum)
        (handler-case (%csl-code-char -1) (error (e) (type-of e)))
        (handler-case (%csl-code-char most-negative-fixnum) (error (e) (type-of e))))
  (#\Nul #\A #\ÿ #\Ā #.(code-char 65535) nil nil type-error type-error))

(deftest char-set-long.evaluation-order
  (%csl-order (make-string 3 :initial-element #\a))
  ((:str :idx :val) "aqa"))

(deftest-compiled-only char-set-long.no-allocation
  (let ((s (make-string 200000)))
    (%csl-fill-into s 1000)
    (list (equal (subseq (%csl-fill-into s 5) 0 5) "abcde")
          (< (min-allocated-bytes (lambda () (%csl-fill-into s 200000))) 20000)))
  (t t))
