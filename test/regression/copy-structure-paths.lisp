;;; COPY-STRUCTURE builds the new slot storage with an explicit allocate-and-copy
;;; rather than Array.Clone, which is a runtime call that reads the array's type
;;; at run time. Same result, so what these pin is that the copy is a copy: the
;;; two instances share no storage, through the copier the DEFSTRUCT defines and
;;; through COPY-STRUCTURE itself.

(defstruct (cps (:constructor cps-mk (a b c))) a b c)
(defstruct cps-big n q r s u v w x y)

(deftest copy-structure.copier-is-independent
  (let* ((o (cps-mk 1 2 3))
         (c (copy-cps o)))
    (setf (cps-a c) :new)
    (list (cps-a o) (cps-a c) (cps-b c) (cps-c c)))
  (1 :new 2 3))

(deftest copy-structure.generic-is-independent
  (let* ((o (cps-mk 1 2 3))
         (c (copy-structure o)))
    (setf (cps-b c) :new)
    (list (cps-b o) (cps-b c) (typep c 'cps)))
  (2 :new t))

(deftest copy-structure.mutating-the-original-after-copying
  (let* ((o (cps-mk 1 2 3))
         (c (copy-structure o)))
    (setf (cps-c o) :orig)
    (list (cps-c o) (cps-c c)))
  (:orig 3))

(deftest copy-structure.large-structure
  (let* ((o (make-cps-big :n 1 :q 2 :r 3 :s 4 :u 5 :v 6 :w 7 :x 8 :y 9))
         (c (copy-cps-big o)))
    (setf (cps-big-y c) :new)
    (list (cps-big-n c) (cps-big-u c) (cps-big-y o) (cps-big-y c) (equalp o c)))
  (1 5 9 :new nil))

(deftest copy-structure.zero-and-one-slot
  (let* ((z (make-cps-big)) (zc (copy-structure z)))
    (list (cps-big-n zc) (equalp z zc)))
  (nil t))

(deftest copy-structure.shared-substructure-stays-shared
  (let* ((inner (list 1 2))
         (o (cps-mk inner :b :c))
         (c (copy-structure o)))
    (setf (car (cps-a c)) :mutated)
    (list (car (cps-a o)) (eq (cps-a o) (cps-a c))))
  (:mutated t))

(deftest copy-structure.rejects-non-structure
  (handler-case (copy-structure 42)
    (type-error () :type-error)
    (error () :other))
  :type-error)
