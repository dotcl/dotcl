;;; A one-argument generic function with EQL methods picks among them by the
;;; argument: a fixnum by its value against the fixnum keys, anything else by
;;; identity against the other keys. The calls go through a compiled call site,
;;; warmed first, so the cached pick is what answers.

(defgeneric gek-pick (x))
(defmethod gek-pick ((x (eql 0))) :zero)
(defmethod gek-pick ((x (eql -1))) :minus-one)
(defmethod gek-pick ((x (eql 100000))) :big)
(defmethod gek-pick ((x (eql #.most-positive-fixnum))) :mpf)
(defmethod gek-pick ((x (eql #.most-negative-fixnum))) :mnf)
(defmethod gek-pick ((x (eql :k))) :keyword)
(defmethod gek-pick (x) (list :default x))
(defun gek-call-pick (x) (let ((r (gek-pick x))) (if (consp r) (car r) r)))
(defun gek-warm (fn args) (dotimes (k 50) (dolist (a args) (funcall fn a))))

(deftest gf-eql-fixnum-keys.values
  (progn
    (gek-warm #'gek-call-pick '(0 5 :k))
    (mapcar #'gek-call-pick
            (list 0 -1 1 (parse-integer "100000") 100001
                  (parse-integer (princ-to-string most-positive-fixnum))
                  (parse-integer (princ-to-string most-negative-fixnum))
                  (1+ most-positive-fixnum) (1- most-negative-fixnum)
                  :k :j 0.0 -1.0 #\a nil)))
  (:zero :minus-one :default :big :default :mpf :mnf :default :default
   :keyword :default :default :default :default :default))

;;; Only EQL methods: a fixnum that matches no key has no applicable method.
(defgeneric gek-only (x))
(defmethod gek-only ((x (eql 1))) :one)
(defmethod gek-only ((x (eql 2))) :two)
(defun gek-call-only (x) (gek-only x))

(deftest gf-eql-fixnum-keys.no-default
  (progn
    (gek-warm #'gek-call-only '(1 2))
    (list (gek-call-only 1) (gek-call-only 2)
          (handler-case (gek-call-only 3) (error () :no-method))
          (handler-case (gek-call-only :one) (error () :no-method))))
  (:one :two :no-method :no-method))

;;; Adding, redefining and removing an EQL method after the site is warm.
(defgeneric gek-change (x))
(defmethod gek-change ((x (eql 0))) :zero)
(defmethod gek-change ((x integer)) :integer)
(defun gek-call-change (x) (gek-change x))

(deftest gf-eql-fixnum-keys.methods-change
  (let ((out '()))
    (gek-warm #'gek-call-change '(0 5))
    (push (mapcar #'gek-call-change '(0 5 7)) out)
    (let ((m (defmethod gek-change ((x (eql 5))) :five)))
      (push (mapcar #'gek-call-change '(0 5 7)) out)
      (gek-warm #'gek-call-change '(0 5))
      (defmethod gek-change ((x (eql 0))) :zero2)
      (push (mapcar #'gek-call-change '(0 5 7)) out)
      (remove-method #'gek-change m))
    (push (mapcar #'gek-call-change '(0 5 7)) out)
    (remove-method #'gek-change
                   (find-method #'gek-change nil '((eql 0))))
    (push (mapcar #'gek-call-change '(0 5 7)) out)
    (nreverse out))
  ((:zero :integer :integer) (:zero :five :integer) (:zero2 :five :integer)
   (:zero2 :integer :integer) (:integer :integer :integer)))

;;; The recursion of an EQL-specialized fib, and of one whose argument leaves the
;;; fixnums partway.
(defgeneric gek-fib (x))
(defmethod gek-fib ((x (eql 0))) 1)
(defmethod gek-fib ((x (eql 1))) 1)
(defmethod gek-fib (x) (+ (gek-fib (- x 1)) (gek-fib (- x 2))))

(defgeneric gek-down (x))
(defmethod gek-down ((x (eql #.(- most-positive-fixnum 2)))) :stop)
(defmethod gek-down (x) (car (list (gek-down (- x 1)))))

(deftest gf-eql-fixnum-keys.recursion
  (list (gek-fib 20) (gek-down (+ most-positive-fixnum 2)))
  (10946 :stop))
