;;; A slot store whose declared type is a structure or a simple CL type tests
;;; TYPEP inline first and calls the general check only when that says no. The
;;; values that pass and the TYPE-ERRORs must be the general check's.

(defstruct %ssci-tree)
(defstruct (%ssci-leaf (:include %ssci-tree)))
(declaim (inline %ssci-node))
(defstruct (%ssci-node (:include %ssci-tree) (:constructor %ssci-node (l k r)))
  (l nil :type %ssci-tree :read-only t)
  (k 0 :type integer :read-only t)
  (r nil :type %ssci-tree :read-only t))
(defstruct (%ssci-box (:constructor %ssci-box (s c f)))
  (s nil :type symbol) (c #\a :type character) (f #'car :type function))

(defun %ssci-te (thunk)
  (handler-case (funcall thunk)
    (type-error (e) (list :type-error (type-error-datum e) (type-error-expected-type e)))))

(defun %ssci-mk (l k r) (%ssci-node l k r))
(defun %ssci-k (l k r) (%ssci-node-k (%ssci-node l k r)))

(deftest struct-slot-check-inline.values
  (let ((leaf (make-%ssci-leaf)))
    (list (%ssci-node-k (%ssci-mk leaf 3 (%ssci-mk leaf 4 leaf)))
          (%ssci-k leaf (expt 2 70) leaf)
          (%ssci-te (lambda () (%ssci-mk 1 2 leaf)))
          (%ssci-te (lambda () (%ssci-mk leaf 2.5 leaf)))
          (%ssci-te (lambda () (%ssci-mk leaf 2 nil)))
          (%ssci-te (lambda () (%ssci-k leaf "x" leaf)))
          (%ssci-box-s (%ssci-box 'a #\b #'cdr))
          (%ssci-te (lambda () (%ssci-box "a" #\b #'cdr)))
          (%ssci-te (lambda () (%ssci-box 'a 1 #'cdr)))
          (let ((b (%ssci-box nil #\c #'car)))
            (list (%ssci-te (lambda () (setf (%ssci-box-c b) 3)))
                  (setf (%ssci-box-s b) :k)
                  (%ssci-box-s b)))))
  (3 1180591620717411303424
   (:type-error 1 %ssci-tree)
   (:type-error 2.5 integer)
   (:type-error nil %ssci-tree)
   (:type-error "x" integer)
   a
   (:type-error "a" symbol)
   (:type-error 1 character)
   ((:type-error 3 character) :k :k)))
