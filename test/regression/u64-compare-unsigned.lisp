;;; Comparing a raw (unsigned-byte 64) local with another u64 local, a
;;; non-negative fixnum constant or a fixnum local is an unsigned machine
;;; compare, with no boxing. The answer is the same as the generic comparison
;;; for every value, including a u64 at or above 2^63 (negative as an Int64) and
;;; a negative fixnum. A bignum or a double on the other side keeps the generic
;;; path.

(defmacro %u64c-def (name op)
  `(progn
     ;; u64 vs u64
     (defun ,(intern (format nil "~a-UU" name)) (a b)
       (let ((x (the (unsigned-byte 64) a)) (y (the (unsigned-byte 64) b)))
         (declare (type (unsigned-byte 64) x y))
         (if (,op x y) :t :f)))
     ;; u64 vs fixnum local
     (defun ,(intern (format nil "~a-UF" name)) (a b)
       (let ((x (the (unsigned-byte 64) a)) (n (the fixnum b)))
         (declare (type (unsigned-byte 64) x) (type fixnum n))
         (if (,op x n) :t :f)))
     ;; fixnum local vs u64 (swapped operands)
     (defun ,(intern (format nil "~a-FU" name)) (a b)
       (let ((x (the (unsigned-byte 64) a)) (n (the fixnum b)))
         (declare (type (unsigned-byte 64) x) (type fixnum n))
         (if (,op n x) :t :f)))
     ;; u64 vs constants
     (defun ,(intern (format nil "~a-UC" name)) (a)
       (let ((x (the (unsigned-byte 64) a)))
         (declare (type (unsigned-byte 64) x))
         (list (if (,op x 0) :t :f) (if (,op x 5) :t :f)
               (if (,op x 9223372036854775807) :t :f))))
     ;; u64 vs untyped (bignum, double, ratio stay generic)
     (defun ,(intern (format nil "~a-UG" name)) (a b)
       (let ((x (the (unsigned-byte 64) a)))
         (declare (type (unsigned-byte 64) x))
         (if (,op x b) :t :f)))))

(%u64c-def %u64c-lt <)
(%u64c-def %u64c-le <=)
(%u64c-def %u64c-gt >)
(%u64c-def %u64c-ge >=)
(%u64c-def %u64c-eq =)
(%u64c-def %u64c-ne /=)

(defparameter *u64c-xs*
  '(0 1 5 9223372036854775806 9223372036854775807 9223372036854775808
    9223372036854775809 18446744073709551614 18446744073709551615))
(defparameter *u64c-ns*
  '(-9223372036854775808 -2 -1 0 1 4 5 6 9223372036854775806 9223372036854775807))

(defun %u64c-expect (op a b) (if (funcall op a b) :t :f))

(defmacro %u64c-check (name op)
  (let ((uu (intern (format nil "~a-UU" name))) (uf (intern (format nil "~a-UF" name)))
        (fu (intern (format nil "~a-FU" name))) (uc (intern (format nil "~a-UC" name)))
        (ug (intern (format nil "~a-UG" name))))
    `(append
      (loop for a in *u64c-xs* append
        (loop for b in *u64c-xs*
              unless (eq (,uu a b) (%u64c-expect ',op a b)) collect (list ',uu a b)))
      (loop for a in *u64c-xs* append
        (loop for b in *u64c-ns*
              unless (eq (,uf a b) (%u64c-expect ',op a b)) collect (list ',uf a b)
              unless (eq (,fu a b) (%u64c-expect ',op b a)) collect (list ',fu a b)))
      (loop for a in *u64c-xs*
            unless (equal (,uc a) (list (%u64c-expect ',op a 0) (%u64c-expect ',op a 5)
                                        (%u64c-expect ',op a 9223372036854775807)))
              collect (list ',uc a))
      (loop for a in *u64c-xs* append
        (loop for b in (list (expt 2 64) (1- (expt 2 64)) (- (expt 2 64)) 5.0d0 -1.0d0
                             9223372036854775808.0d0 1/2 (expt 2 70))
              unless (eq (,ug a b) (%u64c-expect ',op a b)) collect (list ',ug a b))))))

(deftest u64-compare.lt (%u64c-check %u64c-lt <) nil)
(deftest u64-compare.le (%u64c-check %u64c-le <=) nil)
(deftest u64-compare.gt (%u64c-check %u64c-gt >) nil)
(deftest u64-compare.ge (%u64c-check %u64c-ge >=) nil)
(deftest u64-compare.eq (%u64c-check %u64c-eq =) nil)
(deftest u64-compare.ne (%u64c-check %u64c-ne /=) nil)
