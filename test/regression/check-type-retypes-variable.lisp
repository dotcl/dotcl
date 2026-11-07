;;; After a leading (CHECK-TYPE VAR <simple array type>) the rest of the body reads VAR
;;; as that type. The check itself, its error and its STORE-VALUE restart are unchanged.

(deftype %ctr-octets () '(simple-array (unsigned-byte 8) (*)))

(defun %ctr-sum (s n)
  (declare (fixnum n))
  (check-type s %ctr-octets)
  (let ((a 0))
    (declare (fixnum a))
    (dotimes (i n a) (incf a (aref s i)))))

(defun %ctr-setq (s)
  (check-type s %ctr-octets)
  (setq s (make-array 3 :element-type '(unsigned-byte 8) :initial-element 9))
  (reduce #'+ s))

;; assigned a value of another type after the check: legal, nothing is declared
(defun %ctr-reassign (s)
  (check-type s %ctr-octets)
  (setq s (list 1 2 3))
  (incf (car s) 10)
  (reduce #'+ s))

(deftest check-type-retypes-variable.reassigned
  (list (%ctr-reassign (make-array 1 :element-type '(unsigned-byte 8)))
        (%ctr-setq (make-array 1 :element-type '(unsigned-byte 8))))
  (16 27))

(deftest check-type-retypes-variable.values
  (list (%ctr-sum (make-array 4 :element-type '(unsigned-byte 8) :initial-element 5) 4)
        (%ctr-setq (make-array 1 :element-type '(unsigned-byte 8))))
  (20 27))

(deftest check-type-retypes-variable.error
  (list (handler-case (%ctr-sum "abc" 3) (type-error () :type-error))
        (handler-case (%ctr-sum (make-array 4 :element-type '(unsigned-byte 16)) 4) (type-error () :type-error))
        (handler-case (%ctr-sum (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 4) 4)
          (type-error () :type-error)))
  (:type-error :type-error :type-error))

(deftest check-type-retypes-variable.store-value
  (handler-bind ((type-error (lambda (c) (declare (ignore c))
                               (invoke-restart 'store-value
                                               (make-array 2 :element-type '(unsigned-byte 8) :initial-element 7)))))
    (%ctr-sum :wrong 2))
  14)

;;; A method body is a block: the checks are found inside it, with &key bounds.
(defgeneric %ctr-upd (st seq &key start end))
(defmethod %ctr-upd ((st integer) (sequence vector) &key (start 0) (end (length sequence)))
  (check-type sequence %ctr-octets)
  (check-type start fixnum)
  (check-type end fixnum)
  (let ((a st))
    (declare (fixnum a))
    (do ((i start (1+ i))) ((>= i end) a) (incf a (aref sequence i)))))

(deftest check-type-retypes-variable.method
  (list (%ctr-upd 1 (make-array 5 :element-type '(unsigned-byte 8) :initial-element 2))
        (%ctr-upd 1 (make-array 5 :element-type '(unsigned-byte 8) :initial-element 2) :start 2 :end 4)
        (handler-case (%ctr-upd 1 (vector 1 2)) (type-error () :type-error))
        (handler-case (%ctr-upd 1 (make-array 3 :element-type '(unsigned-byte 8)) :end 'x)
          (type-error () :type-error)))
  (11 5 :type-error :type-error))
