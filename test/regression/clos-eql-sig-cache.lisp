;;; Generic functions with EQL specializers and two or more required arguments: the
;;; applicable methods depend on the classes AND on which EQL value each position holds.

(defgeneric %esc-f (a b))
(defmethod %esc-f (a b) (list :general a b))
(defmethod %esc-f ((a integer) b) (list :integer a b))
(defmethod %esc-f ((a (eql 1)) b) (list :a-is-1 a b))
(defmethod %esc-f (a (b (eql :x))) (list :b-is-x a b))
(defmethod %esc-f ((a (eql 1)) (b (eql :x))) (list :both a b))
(defmethod %esc-f ((a (eql 2.5)) b) (list :a-is-2.5 a b))
(defmethod %esc-f ((a (eql #\z)) b) (list :a-is-z a b))
(defmethod %esc-f ((a (eql nil)) b) (list :a-is-nil a b))

(defun %esc-calls ()
  (list (%esc-f 1 :x) (%esc-f 1 :y) (%esc-f 2 :x) (%esc-f 2 :y) (%esc-f "s" :x) (%esc-f "s" :y)
        (%esc-f 2.5 :y) (%esc-f 2.5d0 :y) (%esc-f #\z :y) (%esc-f #\y :y) (%esc-f nil :y)
        ;; second round: the same shapes again, hitting whatever was cached
        (%esc-f 1 :x) (%esc-f 1 :y) (%esc-f 2 :x) (%esc-f 2 :y) (%esc-f "s" :x) (%esc-f "s" :y)
        (%esc-f 2.5 :y) (%esc-f 2.5d0 :y) (%esc-f #\z :y) (%esc-f #\y :y) (%esc-f nil :y)))

(deftest clos-eql-sig-cache.methods-by-value
  (let ((r (%esc-calls)))
    (list (mapcar #'car (subseq r 0 11))
          (equal (subseq r 0 11) (subseq r 11))))
  ((:both :a-is-1 :integer :integer :b-is-x :general :a-is-2.5 :general :a-is-z :general :a-is-nil)
   t))

;;; :around / :before / :after and CALL-NEXT-METHOD through the cache
(defvar *%esc-log* nil)
(defgeneric %esc-g (a b))
(defmethod %esc-g :around (a b) (push :around *%esc-log*) (call-next-method))
(defmethod %esc-g :before ((a (eql 1)) b) (push :before-1 *%esc-log*))
(defmethod %esc-g :after (a (b (eql :x))) (push :after-x *%esc-log*))
(defmethod %esc-g (a b) (push :general *%esc-log*) :g)
(defmethod %esc-g ((a (eql 1)) b) (push :one *%esc-log*) (list :one (call-next-method)))

(defun %esc-run (a b)
  (setf *%esc-log* nil)
  (let ((v (%esc-g a b))) (list v (reverse *%esc-log*))))

(deftest clos-eql-sig-cache.combination
  (list (%esc-run 1 :x) (%esc-run 1 :y) (%esc-run 2 :x) (%esc-run 2 :y)
        (%esc-run 1 :x) (%esc-run 2 :y))
  (((:one :g) (:around :before-1 :one :general :after-x))
   ((:one :g) (:around :before-1 :one :general))
   (:g (:around :general :after-x))
   (:g (:around :general))
   ((:one :g) (:around :before-1 :one :general :after-x))
   (:g (:around :general))))

;;; a method added later is seen by calls that were cached
(defgeneric %esc-h (a b))
(defmethod %esc-h (a b) :old)
(defmethod %esc-h ((a (eql 7)) b) :seven)

(deftest clos-eql-sig-cache.redefinition
  (let ((before (list (%esc-h 7 0) (%esc-h 8 0) (%esc-h 7 0))))
    (eval '(defmethod %esc-h ((a (eql 8)) b) :eight))
    (eval '(defmethod %esc-h (a (b (eql 0))) :zero))
    (list before (list (%esc-h 7 0) (%esc-h 8 0) (%esc-h 9 0) (%esc-h 9 1))))
  ((:seven :old :seven) (:seven :eight :zero :old)))

;;; the keyword check still happens
(defgeneric %esc-k (a b &key))
(defmethod %esc-k ((a (eql 1)) b &key x) (list :one x))
(defmethod %esc-k (a b &key x) (list :other x))

(deftest clos-eql-sig-cache.keywords
  (list (%esc-k 1 2 :x 3) (%esc-k 5 2 :x 3) (%esc-k 1 2 :x 4)
        (handler-case (%esc-k 1 2 :y 3) (error () :error))
        (handler-case (%esc-k 1 2 :y 3) (error () :error)))
  ((:one 3) (:other 3) (:one 4) :error :error))
