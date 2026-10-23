;;; A structure slot declared with a COMMON-LISP float or rational type name
;;; (DOUBLE-FLOAT, SINGLE-FLOAT, FLOAT, REAL, RATIONAL, RATIO, NUMBER) accepts
;;; the values of that type and signals TYPE-ERROR for the rest, alone and
;;; inside a compound type or a DEFTYPE.

(deftype %stfn-real-or-nil () '(or null real))

(defstruct %stfn
  (d 0d0 :type double-float)
  (s 0f0 :type single-float)
  (f 0d0 :type float)
  (r 0 :type real)
  (q 0 :type rational)
  (o 1/2 :type ratio)
  (n 0 :type number)
  (m nil :type %stfn-real-or-nil))

(defun %stfn-try (slot v)
  (handler-case
      (let ((x (ecase slot
                 (:d (make-%stfn :d v)) (:s (make-%stfn :s v)) (:f (make-%stfn :f v))
                 (:r (make-%stfn :r v)) (:q (make-%stfn :q v)) (:o (make-%stfn :o v))
                 (:n (make-%stfn :n v)) (:m (make-%stfn :m v)))))
        (declare (ignore x))
        :ok)
    (type-error () :type-error)))

(deftest slot-type-float-names.accept-reject
  (mapcar (lambda (case)
            (destructuring-bind (slot . vals) case
              (mapcar (lambda (v) (%stfn-try slot v)) vals)))
          (list (list :d 1d0 1f0 1 nil)
                (list :s 1f0 1d0 1/2)
                (list :f 1d0 1f0 1 1/2)
                (list :r 1 (expt 2 70) 1/2 1f0 1d0 #c(1 2) nil)
                (list :q 1 (expt 2 70) 1/2 1d0)
                (list :o 1/2 1 1d0)
                (list :n 1 1/2 1d0 #c(1 2) "1")
                (list :m nil 3 2d0 #\a)))
  ((:ok :type-error :type-error :type-error)
   (:ok :type-error :type-error)
   (:ok :ok :type-error :type-error)
   (:ok :ok :ok :ok :ok :type-error :type-error)
   (:ok :ok :ok :type-error)
   (:ok :type-error :type-error)
   (:ok :ok :ok :ok :type-error)
   (:ok :ok :ok :type-error)))

(deftest slot-type-float-names.setf
  (let ((x (make-%stfn)))
    (list (progn (setf (%stfn-d x) 2d0) (%stfn-d x))
          (handler-case (progn (setf (%stfn-d x) 2) :stored) (type-error () :type-error))
          (handler-case (progn (setf (%stfn-r x) #c(0 1)) :stored) (type-error () :type-error))))
  (2d0 :type-error :type-error))
