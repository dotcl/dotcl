;;; The long form of DEFSETF takes a defsetf lambda list (CLHS 3.4.7):
;;; &optional with a default and supplied-p, &rest and &key. An optional
;;; parameter with a default was spliced into the expander's LAMBDA as a
;;; required parameter, and &key was not handled. McCLIM's DEFMETHOD* expands to
;;; (defsetf f (point &optional return-polar &key coordinates) (nx ny) ...).

(defvar *dsll-store* (make-hash-table :test 'equal))
(defun dsll-pos (p &optional r &key (c :cart)) (gethash (list p r c) *dsll-store*))
(defsetf dsll-pos (p &optional (r nil r-p) &key (c :cart)) (nx ny)
  `(progn (setf (gethash (list ,p ,r ,c) *dsll-store*) (list ,nx ,ny ,r-p))
          (values ,nx ,ny)))
(defsetf dsll-rest (a &rest r) (v) `(list ,a (list ,@r) ,v))
(defsetf dsll-simple (a b) (v) `(list ,a ,b ,v))

(deftest defsetf-lambda-list.optional-default
  (list (multiple-value-list (setf (dsll-pos 1) (values 10 20)))
        (gethash '(1 nil :cart) *dsll-store*))
  ((10 20) (10 20 nil)))

(deftest defsetf-lambda-list.optional-and-key
  (progn (setf (dsll-pos 2 t :c :polar) (values 3 4))
         (values (gethash '(2 t :polar) *dsll-store*)))
  (3 4 t))

(deftest defsetf-lambda-list.rest
  (setf (dsll-rest 1 2 3) 4)
  (1 (2 3) 4))

(deftest defsetf-lambda-list.evaluates-once
  (let ((i 0))
    (list (setf (dsll-simple (incf i) (incf i)) (incf i)) i))
  ((1 2 3) 3))
