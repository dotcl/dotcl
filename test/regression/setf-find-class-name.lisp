;;; (SETF FIND-CLASS) takes a symbol as the class name (CLHS FIND-CLASS). Any
;;; other object was turned into a symbol by its printed name and registered, so
;;; (setf (find-class <class T>) ...) succeeded. SBCL signals a TYPE-ERROR
;;; (ILLEGAL-CLASS-NAME-ERROR). enhanced-find-class's tests check this.

(defun %sfcn-try (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e) (list :type-error (type-error-expected-type e)))
    (error (e) (list :other (type-of e)))))

(deftest setf-find-class-name.class-object
  (let ((c (find-class 't)))
    (list (%sfcn-try (lambda () (setf (find-class c) c)))
          (%sfcn-try (lambda () (setf (find-class c) nil)))
          (eq (find-class 't) c)))
  ((:type-error symbol) (:type-error symbol) t))

(deftest setf-find-class-name.string-and-number
  (list (%sfcn-try (lambda () (setf (find-class "SFCN-STRING") nil)))
        (%sfcn-try (lambda () (setf (find-class 42) (find-class 't)))))
  ((:type-error symbol) (:type-error symbol)))

;;; A symbol still works, including removing the entry with NIL.
(deftest setf-find-class-name.symbol-still-works
  (let ((c (find-class 'standard-object)))
    (list (eq c (setf (find-class '%sfcn-alias) c))
          (eq c (find-class '%sfcn-alias))
          (setf (find-class '%sfcn-alias) nil)
          (find-class '%sfcn-alias nil)))
  (t t nil nil))
