;;; A structure slot whose declared type is made of simple tests (type names with a
;;; direct predicate, integer ranges, OR / AND / NOT of those, MEMBER, DEFTYPEs that
;;; expand to those) is tested inline before the general check. Values of the type
;;; are accepted, and every other value still signals a TYPE-ERROR.

(deftype %ssti-pos () '(integer 0 1000))
(defstruct %ssti-a (x 0 :type t))
(defstruct %ssti-b (x 0 :type fixnum))
(defstruct %ssti-c (x 0 :type (integer 0 255)))
(defstruct %ssti-d (x (make-%ssti-a) :type %ssti-a))
(defstruct %ssti-e (x nil :type (or null %ssti-a)))
(defstruct %ssti-f (x nil :type list))
(defstruct %ssti-g (x #'car :type function))
(defstruct %ssti-h (x 0 :type %ssti-pos))
(defstruct %ssti-i (x nil :type (or function %ssti-pos)))
(defstruct %ssti-j (x nil :type (or structure-object list string condition)))
(defstruct %ssti-k (x nil :type (or null (integer 0 100))))
(defstruct %ssti-l (x nil :type hash-table))
(defstruct %ssti-m (x :a :type (member :a :b nil 3)))
(defstruct %ssti-n (x 0 :type (or fixnum null)))
(defstruct %ssti-o (x 0 :type (unsigned-byte 32)))
(defstruct %ssti-p (x 0 :type (and integer (not (eql 5)))))

(defmacro %ssti-ok-bad (ctor oks bads)
  `(list (every (lambda (v) (handler-case (progn (,ctor :x v) t) (error () nil))) ',oks)
         (every (lambda (v) (handler-case (progn (,ctor :x v) nil) (type-error () t) (error () nil))) ',bads)))

(deftest struct-slot-type-inline.accept-reject
  (list (%ssti-ok-bad make-%ssti-b (0 -5 4611686018427387904 9223372036854775807) (1.5 "x" nil 9223372036854775808))
        (%ssti-ok-bad make-%ssti-c (0 255) (256 -1 "x" 1.0))
        (%ssti-ok-bad make-%ssti-e (nil) (1 "x"))
        (%ssti-ok-bad make-%ssti-f (nil (1 2)) (1 "x" #(1)))
        (%ssti-ok-bad make-%ssti-h (0 1000) (-1 1001 "x" 1.5))
        (%ssti-ok-bad make-%ssti-k (nil 0 100) (101 -1 "x"))
        (%ssti-ok-bad make-%ssti-m (:a :b nil 3) (:c 4 "x"))
        (%ssti-ok-bad make-%ssti-n (nil 5 -7) ("x" 1.5))
        (%ssti-ok-bad make-%ssti-o (0 4294967295) (4294967296 -1))
        (%ssti-ok-bad make-%ssti-p (0 6 -3) (5 "x")))
  ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

(deftest struct-slot-type-inline.classes
  (list (handler-case (progn (make-%ssti-d :x (make-%ssti-a)) t) (error () nil))
        (handler-case (progn (make-%ssti-d :x 1) nil) (type-error () t))
        (handler-case (progn (make-%ssti-e :x (make-%ssti-a)) t) (error () nil))
        (handler-case (progn (make-%ssti-g :x #'cdr) (make-%ssti-g :x (lambda ())) t) (error () nil))
        (handler-case (progn (make-%ssti-g :x 'car) nil) (type-error () t))
        (handler-case (progn (make-%ssti-i :x #'car) (make-%ssti-i :x 7) t) (error () nil))
        (handler-case (progn (make-%ssti-i :x "x") nil) (type-error () t))
        (handler-case (progn (make-%ssti-j :x (make-%ssti-a)) (make-%ssti-j :x "s") (make-%ssti-j :x '(1)) t) (error () nil))
        (handler-case (progn (make-%ssti-j :x 1) nil) (type-error () t))
        (handler-case (progn (make-%ssti-l :x (make-hash-table)) t) (error () nil))
        (handler-case (progn (make-%ssti-l :x 1) nil) (type-error () t)))
  (t t t t t t t t t t t))

(deftest struct-slot-type-inline.setf-and-message
  (let ((s (make-%ssti-c :x 1)))
    (list (progn (setf (%ssti-c-x s) 200) (%ssti-c-x s))
          (handler-case (progn (setf (%ssti-c-x s) 300) :no-error)
            (type-error (c) (and (search "slot X" (princ-to-string c)) :type-error)))
          (%ssti-c-x s)))
  (200 :type-error 200))
