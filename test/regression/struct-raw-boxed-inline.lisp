;;; A structure of up to four slots with both raw (FIXNUM / DOUBLE-FLOAT typed) and boxed slots keeps its
;;; boxed values in its own fields: reads, writes, COPY-STRUCTURE, EQUALP, printing, :INCLUDE and a constructor
;;; whose argument is not provably a fixnum all agree with the array form, and a make allocates no array.

(defstruct (%srb-node (:constructor %srb-make (left right value size)))
  left right value (size 0 :type fixnum))

(defstruct (%srb-pt (:constructor %srb-pt (name x y)))
  name (x 0d0 :type double-float) (y 0d0 :type double-float))

(defstruct (%srb-two (:constructor %srb-two (a n))) a (n 0 :type fixnum))

(defstruct (%srb-base (:constructor %srb-base (a n))) a (n 0 :type fixnum))
(defstruct (%srb-sub (:include %srb-base) (:constructor %srb-sub (a n b))) b)

(defun %srb-build (k) (declare (fixnum k)) (%srb-make :l :r (list k) (+ k 1)))

(deftest struct-raw-boxed-inline.accessors
  (let* ((n (%srb-build 5)) (c (copy-%srb-node n)))
    (setf (%srb-node-left n) :new (%srb-node-size c) 99)
    (list (%srb-node-left n) (%srb-node-right n) (%srb-node-value n) (%srb-node-size n)
          (%srb-node-left c) (%srb-node-size c)
          (equalp (%srb-build 3) (%srb-build 3)) (equalp (%srb-build 3) (%srb-build 4))
          (typep n '%srb-node) (%srb-node-p n)))
  (:new :r (5) 6 :l 99 t nil t t))

(deftest struct-raw-boxed-inline.double-and-two
  (let ((p (%srb-pt "p" 1.5d0 2.5d0)) (q (%srb-two 'x 41)))
    (setf (%srb-pt-x p) 10d0)
    (incf (%srb-two-n q))
    (list (%srb-pt-name p) (%srb-pt-x p) (%srb-pt-y p) (%srb-two-a q) (%srb-two-n q)
          (%srb-pt-name (copy-%srb-pt p))
          (prin1-to-string (%srb-two 'x 1))))
  ("p" 10d0 2.5d0 x 42 "p" "#S(%SRB-TWO :A X :N 1)"))

(deftest struct-raw-boxed-inline.include
  (let ((s (%srb-sub 'a 7 'b)))
    (list (%srb-base-a s) (%srb-base-n s) (%srb-sub-b s) (typep s '%srb-base)
          (%srb-base-n (copy-%srb-sub s))))
  (a 7 b t 7))

(deftest struct-raw-boxed-inline.type-errors
  (list (handler-case (%srb-two 'x "no") (type-error () :type-error))
        (handler-case (let ((q (%srb-two 'x 1))) (setf (%srb-two-n q) 'z) q) (type-error () :type-error)))
  (:type-error :type-error))

(defun %srb-loop (n) (declare (fixnum n)) (let ((x nil)) (dotimes (i n) (setq x (%srb-build 5))) x))

(deftest-compiled-only struct-raw-boxed-inline.no-array
  (progn (%srb-loop 10)
         ;; a node is the object (48 bytes of header and fields, 32 of raw cells, 32 of boxed cells) and
         ;; the list (32 bytes): an array on top would add 56 more (the limit sits between the two).
         (< (/ (min-allocated-bytes (lambda () (%srb-loop 100000))) 100000.0) 160))
  t)
