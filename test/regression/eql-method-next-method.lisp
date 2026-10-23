;;; EQL-specialized methods, with and without CALL-NEXT-METHOD / NEXT-METHOD-P,
;;; called warm (many times), and after the generic function changes.

(defgeneric emnm (x))
(defmethod emnm ((x (eql 0))) :zero)
(defmethod emnm ((x (eql 1))) (list :one (call-next-method)))
(defmacro %emnm-next () '(call-next-method))
(defmethod emnm ((x (eql 2))) (list :two (%emnm-next)))
(defmethod emnm ((x (eql 3))) (list :three (next-method-p)))
(defmethod emnm ((x (eql :k))) :keyword)
(defmethod emnm ((x (eql nil))) :nil)
(defmethod emnm ((x (eql t))) :t)
(defmethod emnm ((x integer)) (list :integer (call-next-method)))
(defmethod emnm ((x t)) :default)

(defun %emnm-all ()
  (mapcar #'emnm '(0 1 2 3 4 :k :j nil t 1.5)))

(deftest eql-method-next-method.warm
  (let ((first (%emnm-all)))
    (dotimes (i 50) (%emnm-all))
    (list first (%emnm-all)))
  ((:zero (:one (:integer :default)) (:two (:integer :default)) (:three t)
     (:integer :default) :keyword :default :nil :t :default)
    (:zero (:one (:integer :default)) (:two (:integer :default)) (:three t)
     (:integer :default) :keyword :default :nil :t :default)))

(defgeneric emfib (x))
(defmethod emfib ((x (eql 0))) 1)
(defmethod emfib ((x (eql 1))) 1)
(defmethod emfib (x) (+ (emfib (- x 1)) (emfib (- x 2))))

(deftest eql-method-next-method.fib-then-redefine
  (list (emfib 15)
        (progn (defmethod emfib ((x (eql 1))) 2) (emfib 15))
        (progn (defmethod emfib ((x (eql 2))) 0) (emfib 15))
        (progn (remove-method #'emfib (find-method #'emfib '() (list (list 'eql 2))))
               (emfib 15)))
  (987 1597 466 1597))

(defgeneric emonly (x))
(defmethod emonly ((x (eql 5))) :five)

(deftest eql-method-next-method.no-default
  (list (emonly 5) (emonly 5) (emonly 5)
        (handler-case (emonly 6) (error () :no-method))
        (emonly 5))
  (:five :five :five :no-method :five))
