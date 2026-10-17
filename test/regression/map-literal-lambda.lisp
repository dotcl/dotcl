;;; MAPCAR / MAPC / MAPCAN / SOME / EVERY / NOTANY / NOTEVERY called with a
;;; lambda expression written in place open into a loop, and the lambda becomes a
;;; local function that is only called directly. A closure (the function object,
;;; its body delegate and the captured-variable array) was made on every call.
;;; (mapcar (lambda (x) (g y x)) l) over 3 elements cost 328 bytes; the result
;;; conses are 96.

(defun %mll-g (y x) (if (eql x y) :same (list y x)))

(deftest mll.mapcar-basic
  (let ((y 2))
    (list (mapcar (lambda (x) (%mll-g y x)) (list 1 2 3))
          (mapcar #'(lambda (x) (* x 10)) (list 1 2 3))
          (mapcar (lambda (x) x) nil)
          (mapcar (lambda (a b) (+ a b)) (list 1 2 3) (list 10 20))
          (mapcar (lambda (a b c) (list a b c)) '(1 2) '(3 4) '(5 6 7))))
  (((2 1) :same (2 3)) (10 20 30) nil (11 22) ((1 3 5) (2 4 6))))

;;; Only the primary value of each call is kept, and the call yields one value.
(deftest mll.single-values
  (list (multiple-value-list (mapcar (lambda (x) (values x :extra)) (list 1 2)))
        (multiple-value-list (mapcan (lambda (x) (values (list x) :extra)) (list 1 2)))
        (multiple-value-list (every (lambda (x) (values x :extra)) (list 1 2))))
  (((1 2)) ((1 2)) (t)))

;;; The lambda body sees its own lexical environment: RETURN goes to the
;;; enclosing NIL block, RETURN-FROM and GO leave the mapping, and the
;;; variables of the loop are not visible.
(deftest mll.non-local-exits
  (list (dolist (k '(1) :none)
          (mapc (lambda (x) (when (= x 2) (return (list :returned k x)))) (list 1 2 3)))
        (block outer
          (mapcar (lambda (x) (if (= x 2) (return-from outer :left) x)) (list 1 2 3)))
        (let ((seen nil))
          (tagbody
             (mapc (lambda (x) (push x seen) (when (= x 2) (go out))) (list 1 2 3))
           out)
          seen)
        (block nil
          (some (lambda (x) (when (= x 2) (return :from-some))) (vector 1 2 3))))
  ((:returned 1 2) :left (2 1) :from-some))

;;; Each element gets its own binding: closures made in the body keep theirs.
(deftest mll.fresh-binding-per-element
  (mapcar #'funcall (mapcar (lambda (x) (lambda () x)) (list 1 2 3)))
  (1 2 3))

;;; The body assigns a variable of the enclosing function, and its own.
(deftest mll.assign-outer
  (let ((sum 0) (n 0))
    (mapc (lambda (x) (incf sum x) (setq x (* x 2)) (incf n x)) (list 1 2 3))
    (list sum n))
  (6 12))

;;; The list forms are evaluated in order, once.
(deftest mll.evaluation-order
  (let ((log nil))
    (list (mapcar (lambda (a b) (cons a b))
                  (progn (push :first log) (list 1 2))
                  (progn (push :second log) (list 3 4)))
          (reverse log)))
  (((1 . 3) (2 . 4)) (:first :second)))

;;; &OPTIONAL, &REST and &AUX in the lambda list; a lambda list that does not
;;; accept the arguments is still an error when called.
(deftest mll.lambda-lists
  (list (mapcar (lambda (a &optional (b :none b-p)) (list a b b-p)) (list 1 2))
        (mapcar (lambda (a &optional b) (list a b)) (list 1 2) (list 3 4))
        (mapcar (lambda (&rest xs) xs) (list 1 2) (list 3 4))
        (mapcar (lambda (a &aux (b (* a 2))) b) (list 1 2))
        (mapcar (lambda (x) "a docstring" (declare (fixnum x)) (1+ x)) (list 1 2))
        (mapcar (lambda (x) "only a string") (list 1))
        (handler-case (mapcar (lambda (a b) (list a b)) (list 1 2))
          (error () :error))
        (handler-case (mapcar (lambda (&key k) k) (list :k) (list 1))
          (error () :error)))
  (((1 :none nil) (2 :none nil)) ((1 3) (2 4)) ((1 3) (2 4)) (2 4) (2 3) ("only a string")
   :error (1)))

;;; Lists that are not proper.
(deftest mll.improper
  (list (handler-case (mapcar (lambda (x) x) 5) (type-error () :type-error))
        (handler-case (mapcar (lambda (x) x) '(1 2 . 3)) (type-error () :type-error))
        (handler-case (mapc (lambda (x) x) '(1 . 2)) (type-error () :type-error))
        (handler-case (some (lambda (x) (eql x 9)) '(1 . 2)) (type-error () :type-error))
        (handler-case (every (lambda (x) x) 7) (type-error () :type-error)))
  (:type-error :type-error :type-error :type-error :type-error))

(deftest mll.mapc-returns-first-list
  (let ((l (list 1 2)) (acc nil))
    (list (eq l (mapc (lambda (x) (push x acc)) l))
          (eq l (mapc (lambda (x y) (push (+ x y) acc)) l (list 10 20 30)))
          acc))
  (t t (22 11 2 1)))

(deftest mll.mapcan
  (list (mapcan (lambda (x) (if (evenp x) (list x x) nil)) (list 1 2 3 4))
        (mapcan (lambda (x) (list x)) nil)
        (mapcan (lambda (x) (if (= x 3) :end (list x))) (list 1 2 3))
        (handler-case (mapcan (lambda (x) (if (= x 1) :atom (list x))) (list 1 2))
          (type-error () :type-error))
        (mapcan (lambda (a b) (list (+ a b))) (list 1 2) (list 10 20 30)))
  ((2 2 4 4) nil (1 2 . :end) :type-error (11 22)))

(deftest mll.some-every
  (list (some (lambda (x) (and (> x 1) (* x 10))) (list 1 2 3))
        (some (lambda (x) (and (> x 1) (* x 10))) (vector 1 2 3))
        (some (lambda (c) (and (char= c #\b) :b)) "abc")
        (some (lambda (x) x) nil)
        (every (lambda (x) (> x 0)) (list 1 2 3))
        (every (lambda (x) (> x 1)) (vector 1 2 3))
        (every (lambda (x) x) "")
        (notany (lambda (x) (> x 5)) (list 1 2))
        (notany (lambda (x) (> x 1)) (vector 1 2))
        (notevery (lambda (x) (> x 1)) (list 1 2))
        (notevery (lambda (x) (> x 0)) (vector 1 2))
        (some (lambda (x) (> x 2)) (make-array 5 :initial-contents '(1 2 3 4 5) :fill-pointer 2))
        (some (lambda (a b) (and (> (+ a b) 20) (+ a b))) (list 1 2) (vector 10 20)))
  (20 20 :b nil t nil t t nil t nil nil 22))

;;; The opened loop where its value is used in different ways: as a function's
;;; value, as a test, under MULTIPLE-VALUE-LIST, discarded, and nested in
;;; another opened loop's lambda.
(defun %mll-tail (l k) (mapcar (lambda (x) (+ x k)) l))

(deftest mll.value-contexts
  (let ((k 1) (log nil))
    (list (%mll-tail (list 1 2) 10)
          (if (some (lambda (x) (> x k)) (vector 0 1 2)) :yes :no)
          (if (every (lambda (x) (> x k)) (list 2 0)) :yes :no)
          (multiple-value-list (some (lambda (x) (values x :extra)) (list nil 3)))
          (multiple-value-list (notevery (lambda (x) (values x :extra)) (vector 1 nil)))
          (progn (mapc (lambda (x) (push x log)) (list 1 2)) log)
          (mapcar (lambda (row) (mapcar (lambda (x) (* x k 2)) row))
                  (list (list 1 2) (list 3)))
          (mapcan (lambda (row) (remove-if-not (lambda (x) (some (lambda (y) (= x y)) row))
                                               (list 1 2 3)))
                  (list (list 2) (list 3 1)))))
  ((11 12) :yes :no (3) (t) (2 1) ((2 4) (6)) (2 1 3)))

;;; NOTINLINE keeps the ordinary call.
(deftest mll.notinline
  (locally (declare (notinline mapcar))
    (mapcar (lambda (x) (1+ x)) (list 1 2)))
  (2 3))

;;; --- the point: no closure ---

(defun %mll-bytes () (nth 4 (dotcl:gc-stats)))

(defvar *mll-l* (list 1 2 3))

(defmacro %mll-per-call (name &body body)
  `(progn
     (defun ,name (n y)
       (declare (fixnum n))
       (let ((r nil))
         (do ((i 0 (1+ i))) ((= i n) r)
           (declare (fixnum i))
           (setq r (progn ,@body)))))
     (,name 2000 1)
     (let ((best nil))
       (dotimes (k 5 best)
         (let ((before (%mll-bytes)))
           (,name 50000 1)
           (let ((used (- (%mll-bytes) before)))
             (when (or (null best) (< used best)) (setq best used))))))))

;;; Within one byte per call of building the same conses by hand.
(deftest-compiled-only mll.mapcar-costs-only-its-conses
  (<= (abs (- (%mll-per-call %mll-mapcar (mapcar (lambda (x) (%mll-g y x)) *mll-l*))
              (%mll-per-call %mll-hand (list (%mll-g y 1) (%mll-g y 2) (%mll-g y 3)))))
      50000)
  t)

(deftest-compiled-only mll.mapc-some-every-allocate-nothing
  (list (<= (%mll-per-call %mll-mapc (mapc (lambda (x) (eql x y)) *mll-l*)) 50000)
        (<= (%mll-per-call %mll-some (some (lambda (x) (and (eql x 99) y)) *mll-l*)) 50000)
        (<= (%mll-per-call %mll-every (every (lambda (x) (or (eql x 99) y)) *mll-l*)) 50000))
  (t t t))

;;; RETURN-FROM / RETURN / GO out of the lambda body is a jump, not a thrown exit.
(defgeneric %mll-gf-exit (xs))
(defmethod %mll-gf-exit ((xs list))
  (mapcar (lambda (x) (when (null x) (return-from %mll-gf-exit :none)) (car x)) xs))

(deftest mll.exit-from-method-and-optional
  (list (%mll-gf-exit (list '(1) '(2)))
        (%mll-gf-exit (list '(1) nil '(2)))
        (block b (mapcar (lambda (x &optional (y 10)) (if (> x 1) (return-from b (+ x y)) x))
                         (list 1 2 3)))
        (block b (mapc (lambda (x) (mapc (lambda (z) (when (= z x 2) (return-from b z))) (list 2)))
                       (list 1 2)))
        (funcall (block b (mapcar (lambda (x) (when (= x 3) (return-from b (lambda () x))) x) (list 1 3)))))
  ((1 2) :none 12 2 3))

(deftest-compiled-only mll.exit-allocates-no-exception
  (<= (%mll-per-call %mll-exit
        (block b (mapcar (lambda (x) (if (eql x 2) (return-from b y) x)) *mll-l*)))
      (* 50000 40))
  t)
