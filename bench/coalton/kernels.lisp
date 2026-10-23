;;;; Small Coalton programs, each a typical shape of Coalton code.
;;;; Every kernel returns a number that run.lisp prints as a checksum, so the
;;;; two implementations can be checked against each other.

(defpackage #:coalton-bench
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:list #:coalton/list)
                    (#:string #:coalton/string)
                    (#:cell #:coalton/cell)
                    (#:int #:coalton/math/integral)))

(in-package #:coalton-bench)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  ;; Type-class dispatch: one polymorphic loop, used at two types.

  (declare poly-sum ((Num :a) (Ord :a) => :a * :a * :a -> :a))
  (define (poly-sum start step limit)
    (rec % ((x start) (acc start))
      (if (> x limit)
          acc
          (% (+ x step) (+ acc (* x x))))))

  (declare poly-sum-integer (Integer -> Integer))
  (define (poly-sum-integer n)
    (poly-sum 1 1 n))

  (declare poly-sum-f64 (Integer -> F64))
  (define (poly-sum-f64 n)
    (poly-sum 1.0d0 1.0d0 (fromInt n)))

  ;; A user-defined class, dispatched over a list.

  (define-class (Shape :s)
    (area (:s -> F64))
    (perimeter (:s -> F64)))

  (define-struct Circle (radius F64))
  (define-struct Rect (width F64) (height F64))

  (define-instance (Shape Circle)
    (define (area c) (* 3.141592653589793d0 (* (.radius c) (.radius c))))
    (define (perimeter c) (* 6.283185307179586d0 (.radius c))))

  (define-instance (Shape Rect)
    (define (area r) (* (.width r) (.height r)))
    (define (perimeter r) (* 2.0d0 (+ (.width r) (.height r)))))

  (declare measure ((Shape :s) => List :s -> F64))
  (define (measure shapes)
    (fold (fn (acc s) (+ acc (+ (area s) (perimeter s)))) 0.0d0 shapes))

  (declare class-dispatch (Integer -> F64))
  (define (class-dispatch rounds)
    (let circles = (map (fn (i) (Circle (fromInt i))) (list:range 1 1000)))
    (let rects = (map (fn (i) (Rect (fromInt i) 2.0d0)) (list:range 1 1000)))
    (rec % ((r rounds) (acc 0.0d0))
      (if (== r 0)
          acc
          (% (- r 1) (+ acc (+ (measure circles) (measure rects)))))))

  ;; Eq / Ord through the library: sort a list of integers.

  (declare lcg (Integer -> Integer))
  (define (lcg x)
    (mod (+ (* x 1103515245) 12345) 2147483648))

  (declare random-list (Integer * Integer -> List Integer))
  (define (random-list seed n)
    (rec % ((i n) (x seed) (acc Nil))
      (if (== i 0)
          acc
          (% (- i 1) (lcg x) (Cons (mod x 1000000) acc)))))

  (declare ord-sort (Integer -> Integer))
  (define (ord-sort n)
    (let sorted = (list:sort (random-list 42 n)))
    (fold (fn (acc x) (mod (+ (* acc 31) x) 1000000007)) 0 sorted))

  ;; ADT construction and pattern matching: an unbalanced search tree.

  (define-type Tree
    Leaf
    (Node Tree Integer Tree))

  (declare insert (Tree * Integer -> Tree))
  (define (insert tree key)
    (match tree
      ((Leaf) (Node Leaf key Leaf))
      ((Node l k r)
       (cond ((< key k) (Node (insert l key) k r))
             ((> key k) (Node l k (insert r key)))
             (True tree)))))

  (declare tree-sum (Tree -> Integer))
  (define (tree-sum tree)
    (match tree
      ((Leaf) 0)
      ((Node l k r) (+ k (+ (tree-sum l) (tree-sum r))))))

  (declare tree-depth (Tree -> Integer))
  (define (tree-depth tree)
    (match tree
      ((Leaf) 0)
      ((Node l _ r) (+ 1 (max (tree-depth l) (tree-depth r))))))

  (declare tree-insert-fold (Integer -> Integer))
  (define (tree-insert-fold n)
    (let tree = (fold insert Leaf (random-list 7 n)))
    (+ (tree-sum tree) (tree-depth tree)))

  ;; Optional and Result chains.

  (declare safe-div (Integer * Integer -> Optional Integer))
  (define (safe-div a b)
    (if (== b 0) None (Some (int:div a b))))

  (declare check-small (Integer -> Result String Integer))
  (define (check-small x)
    (if (< x 1000) (Ok x) (Err "too large")))

  (declare optional-chain (Integer -> Integer))
  (define (optional-chain n)
    (rec % ((i 0) (acc 0))
      (if (== i n)
          acc
          (let ((step (>>= (safe-div 1000000 (mod i 97))
                           (fn (q) (>>= (safe-div q (+ 1 (mod i 13)))
                                        (fn (r) (Some (+ r 1))))))))
            (% (+ i 1)
               (match step
                 ((Some v) (+ acc v))
                 ((None) (- acc 1))))))))

  (declare result-chain (Integer -> Integer))
  (define (result-chain n)
    (rec % ((i 0) (acc 0))
      (if (== i n)
          acc
          (let ((step (>>= (check-small (mod i 1500))
                           (fn (x) (map (fn (y) (* y 2)) (check-small (+ x 300)))))))
            (% (+ i 1)
               (match step
                 ((Ok v) (+ acc v))
                 ((Err _) (+ acc 1))))))))

  ;; List operations: map, filter, fold over Coalton lists.

  (declare list-ops (Integer -> Integer))
  (define (list-ops rounds)
    (let xs = (list:range 1 100000))
    (rec % ((r rounds) (acc 0))
      (if (== r 0)
          acc
          (% (- r 1)
             (+ acc (fold + 0 (list:filter (fn (x) (== 0 (mod x 3)))
                                           (map (fn (x) (+ (* x 7) r)) xs))))))))

  ;; Integer arithmetic: naive recursion.

  (declare fib (Integer -> Integer))
  (define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))

  ;; Floating point loops.

  (declare mandel-point (F64 * F64 * UFix -> UFix))
  (define (mandel-point cr ci limit)
    (rec % ((zr 0.0d0) (zi 0.0d0) (k 0))
      (if (or (== k limit) (> (+ (* zr zr) (* zi zi)) 4.0d0))
          k
          (% (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0d0 (* zr zi)) ci) (+ k 1)))))

  (declare mandel-f64 (UFix -> UFix))
  (define (mandel-f64 size)
    (let scale = (/ 3.0d0 (fromInt (into size))))
    (for ((y (the UFix 0) (+ y 1))
          (acc (the UFix 0)
               (for ((x (the UFix 0) (+ x 1))
                     (row acc
                          (+ row (mandel-point (- (* scale (fromInt (into x))) 2.0d0)
                                               (- (* scale (fromInt (into y))) 1.5d0)
                                               100))))
                 :returns row
                 :repeat size
                 (values))))
      :returns acc
      :repeat size
      (values)))

  (declare f32-series (UFix -> F32))
  (define (f32-series n)
    (rec % ((i (the UFix 0)) (x 0.0f0) (acc 0.0f0))
      (if (== i n)
          acc
          (% (+ i 1)
             (if (> x 1.0f0) 0.0f0 (+ x 0.001f0))
             (+ (* acc 0.999f0) (/ (* x x) (+ 1.0f0 (* x x))))))))

  ;; String building.

  (declare string-build (Integer -> Integer))
  (define (string-build n)
    (rec % ((i 0) (acc 0))
      (if (== i n)
          acc
          (let ((s (string:concat "key-" (into i)))
                (t (<> s (<> ":" (string:reverse s)))))
            (% (+ i 1) (+ acc (into (string:length t)))))))))

(cl:defparameter *kernels*
  (cl:list
   (cl:cons "poly-sum-integer" (cl:lambda () (coalton (poly-sum-integer 3000000))))
   (cl:cons "poly-sum-f64"     (cl:lambda () (coalton (poly-sum-f64 3000000))))
   (cl:cons "class-dispatch"   (cl:lambda () (coalton (class-dispatch 2000))))
   (cl:cons "ord-sort"         (cl:lambda () (coalton (ord-sort 200000))))
   (cl:cons "tree-insert-fold" (cl:lambda () (coalton (tree-insert-fold 200000))))
   (cl:cons "optional-chain"   (cl:lambda () (coalton (optional-chain 2000000))))
   (cl:cons "result-chain"     (cl:lambda () (coalton (result-chain 2000000))))
   (cl:cons "list-ops"         (cl:lambda () (coalton (list-ops 50))))
   (cl:cons "fib-integer"      (cl:lambda () (coalton (fib 32))))
   (cl:cons "mandel-f64"       (cl:lambda () (coalton (mandel-f64 900))))
   (cl:cons "f32-series"       (cl:lambda () (coalton (f32-series 10000000))))
   (cl:cons "string-build"     (cl:lambda () (coalton (string-build 200000))))))
