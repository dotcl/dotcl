;;; MAPCAR over two lists costs the same as over one: the result conses, nothing
;;; else.
;;;
;;; The multi-list path used to collect into a List, call ToArray, and hand that
;;; to LIST -- three intermediates for a result it could write directly -- and
;;; build a fresh argument array for the callee on every element, on top of the
;;; array the call site built for the lists and the cursor array MAPCARN kept.
;;; Two lists of three elements cost 663.9 bytes where SBCL costs 47.2, against a
;;; 94.4-byte floor (a .NET cons is twice the size of an SBCL cons).

(deftest mapcar-multi.two-lists
  (list (mapcar #'+ (list 1 2 3) (list 10 20 30))
        (mapcar (lambda (a b) (cons b a)) (list 1 2) (list :a :b)))
  ((11 22 33) ((:a . 1) (:b . 2))))

;;; The shortest list decides the length (CLHS 17.2), from either side.
(deftest mapcar-multi.stops-at-shortest
  (list (mapcar #'+ (list 1 2) (list 10 20 30))
        (mapcar #'+ (list 1 2 3) (list 10 20))
        (mapcar #'+ nil (list 1 2))
        (mapcar #'+ (list 1 2) nil))
  ((11 22) (11 22) nil nil))

(deftest mapcar-multi.three-and-more
  (list (mapcar #'+ (list 1 2) (list 10 20) (list 100 200))
        (mapcar #'list (list 1 2) (list 3 4) (list 5 6) (list 7 8))
        (mapcar #'list (list 1) (list 2) (list 3) (list 4) (list 5)))
  ((111 222) ((1 3 5 7) (2 4 6 8)) ((1 2 3 4 5))))

;;; Only the primary value of the function is collected.
(deftest mapcar-multi.multiple-values
  (mapcar (lambda (a b) (values (+ a b) :extra)) (list 1 2) (list 10 20))
  (11 22))

;;; The list arguments are evaluated left to right, before any element is mapped.
(defvar *mml-side* nil)
(defun %mml-note (x) (push x *mml-side*) x)

(deftest mapcar-multi.argument-order
  (progn
    (setq *mml-side* nil)
    (let ((r (mapcar #'list
                     (list (%mml-note :a) (%mml-note :b))
                     (list (%mml-note :c) (%mml-note :d)))))
      (list r (reverse *mml-side*))))
  (((:a :c) (:b :d)) (:a :b :c :d)))

;;; Reached as a value rather than through the compiler's intrinsic, the answers
;;; are the same.
(deftest mapcar-multi.as-a-value
  (list (apply #'mapcar #'+ (list (list 1 2) (list 10 20)))
        (funcall #'mapcar #'+ (list 1 2) (list 10 20)))
  ((11 22) (11 22)))

;;; A list argument that opens a try block still compiles: the function and both
;;; lists go to temps first, so nothing is on the stack at the try entry. This is
;;; the rule the multi-list path was written around in the first place.
(deftest mapcar-multi.loop-argument
  (mapcar #'+
          (loop :for i :from 1 :to 3 :collect i)
          (loop :for i :from 10 :to 30 :by 10 :collect i))
  (11 22 33))

(deftest mapcar-multi.nested
  (mapcar #'+ (mapcar #'1+ (list 1 2)) (list 10 20))
  (12 23))

;;; --- the point: two lists cost what one list costs ---

(defun %mml-bytes () (nth 4 (dotcl:gc-stats)))

(defvar *mml-a* (list 1 2 3))
(defvar *mml-b* (list 4 5 6))
(defun %mml-k2 (a b) (declare (ignore b)) a)
(defun %mml-k1 (a) a)

(defmacro %mml-per-call (name &body body)
  `(progn
     (defun ,name (n)
       (declare (fixnum n))
       (let ((r nil))
         (do ((i 0 (1+ i))) ((= i n) r)
           (declare (fixnum i))
           (setq r (progn ,@body)))))
     (,name 2000)
     (let ((best nil))
       (dotimes (r 5 best)
         (let ((before (%mml-bytes)))
           (,name 50000)
           (let ((used (- (%mml-bytes) before)))
             (when (or (null best) (< used best)) (setq best used))))))))

;;; Compared against the one-list call on a list of the same length rather than
;;; against a constant, so the test keeps meaning if a cons changes size. Within
;;; one byte per call; what this guards against was 568 bytes per call.
(deftest-compiled-only mapcar-multi.costs-only-its-conses
  (<= (abs (- (%mml-per-call %mml-two (mapcar #'%mml-k2 *mml-a* *mml-b*))
              (%mml-per-call %mml-one (mapcar #'%mml-k1 *mml-a*))))
      50000)
  t)
