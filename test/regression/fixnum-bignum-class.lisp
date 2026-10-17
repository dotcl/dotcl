;;; A fixnum is an instance of the class FIXNUM and a bignum of BIGNUM, so
;;; methods specialized on them apply (cl-robdd defines BDD on FIXNUM).

(deftest fixnum-bignum-class.class-of
  (list (class-name (class-of 1))
        (class-name (class-of most-positive-fixnum))
        (class-name (class-of (1+ most-positive-fixnum)))
        (class-name (class-of (1- most-negative-fixnum))))
  (fixnum fixnum bignum bignum))

(deftest fixnum-bignum-class.precedence
  (mapcar #'class-name (dotcl-mop:class-precedence-list (class-of 1)))
  (fixnum integer rational real number t))

(defgeneric %fbc-kind (x))
(defmethod %fbc-kind ((x fixnum)) :fixnum)
(defmethod %fbc-kind ((x integer)) :integer)
(defmethod %fbc-kind ((x t)) :t)

(deftest fixnum-bignum-class.dispatch
  (list (%fbc-kind 1) (%fbc-kind -7) (%fbc-kind (expt 2 70)) (%fbc-kind 1/2))
  (:fixnum :fixnum :integer :t))

(defgeneric %fbc-int (x))
(defmethod %fbc-int ((x integer)) (list :integer x))

(deftest fixnum-bignum-class.integer-method-still-applies
  (list (%fbc-int 3) (%fbc-int (expt 2 70)))
  ((:integer 3) (:integer 1180591620717411303424)))
