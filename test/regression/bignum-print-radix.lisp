;;; A bignum is printed in any radix by splitting it into halves (Runtime.BigIntToRadixString).
;;; The text is the one the digit-by-digit conversion gives, including the zero padding of the
;;; low halves, the sign, and the numbers on both sides of the size where the split starts.

(defun %bpr-reference (n radix)
  "N in RADIX the slow way, upper-case digits."
  (cond ((zerop n) "0")
        ((minusp n) (concatenate 'string "-" (%bpr-reference (- n) radix)))
        (t (let ((chars '()))
             (loop until (zerop n)
                   do (multiple-value-bind (q r) (floor n radix)
                        (push (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" r) chars)
                        (setf n q)))
             (coerce chars 'string)))))

(defun %bpr-string (n radix)
  (let ((*print-base* radix) (*print-radix* nil)) (princ-to-string n)))

(defun %bpr-lcg-number (digits seed)
  "A deterministic bignum of about DIGITS decimal digits."
  (let ((x seed) (n 0))
    (dotimes (i (ceiling digits 9))
      (setf x (mod (+ (* x 1103515245) 12345) 2147483648))
      (setf n (+ (* n 1000000000) (mod x 1000000000))))
    n))

(defun %bpr-cases ()
  (append
   (list 0 1 -1 9 10 -10 (expt 10 18) (expt 2 62) (expt 2 63) (- (expt 2 63)) (expt 2 64) (1- (expt 2 64)))
   (loop for k in '(100 500 1000 1052 1053 1054 2000 2105 4096)
         append (list (expt 10 k) (1- (expt 10 k)) (1+ (expt 10 k)) (- (expt 10 k))))
   (loop for k in '(3400 3499 3500 3501 3600 7000 7001 10000)
         append (list (expt 2 k) (1- (expt 2 k)) (1+ (expt 2 k)) (- (1- (expt 2 k)))))
   (loop for d in '(30 300 1000 1100 2500 5000)
         for s from 7 by 5
         append (list (%bpr-lcg-number d s) (- (%bpr-lcg-number d (+ s 1)))))))

(deftest bignum-print-radix.same-as-digit-by-digit
  (let ((bad '()))
    (dolist (n (%bpr-cases))
      (dolist (radix '(2 8 10 16 36 7))
        (unless (string= (%bpr-string n radix) (%bpr-reference n radix))
          (push (list n radix) bad))))
    (length bad))
  0)

(deftest bignum-print-radix.print-base-and-radix-and-format
  (let ((n (- (expt 3 5000))))
    (list (string= (format nil "~D" n) (%bpr-reference n 10))
          (string= (format nil "~B" n) (%bpr-reference n 2))
          (string= (format nil "~O" n) (%bpr-reference n 8))
          (string= (format nil "~X" n) (%bpr-reference n 16))
          (string= (format nil "~12R" n) (%bpr-reference n 12))
          (string= (let ((*print-base* 16) (*print-radix* t)) (prin1-to-string (expt 3 5000)))
                   (concatenate 'string "#x" (%bpr-reference (expt 3 5000) 16)))
          (string= (let ((*print-radix* t)) (prin1-to-string (expt 3 5000)))
                   (concatenate 'string (%bpr-reference (expt 3 5000) 10) "."))
          (string= (let ((*print-base* 3)) (prin1-to-string (/ (expt 7 3000) (expt 5 2500))))
                   (concatenate 'string (%bpr-reference (expt 7 3000) 3) "/" (%bpr-reference (expt 5 2500) 3)))
          (= (parse-integer (%bpr-string (expt 7 4000) 10)) (expt 7 4000))
          (= (parse-integer (%bpr-string (expt 7 4000) 36) :radix 36) (expt 7 4000))))
  (t t t t t t t t t t))
