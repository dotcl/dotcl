;;; Regression tests for Int64-slot ("long-rep") let locals: fixnum-declared,
;;; non-captured lexicals whose CIL slot holds a raw int64 instead of a boxed
;;; Fixnum. Loop counters from dotimes are the primary client (the counter
;;; increment and the loop compare must not box), but any fixnum-declared
;;; mutated local takes this representation. Correctness contract:
;;;  - values match the generic boxed path exactly (no silent int64 wrap:
;;;    a store whose range is not provable goes through the promoting boxed
;;;    path and unboxes, so overflow is loud, never wrong)
;;;  - captured counters keep the boxed representation (closures see updates
;;;    per the single-binding dotimes semantics)
;;;  - rebinding the same name as an untyped local shadows the Int64 slot

;;; ---- dotimes counters ----

(deftest long-rep-dotimes-sum
  (let ((s 0))
    (dotimes (i 10) (setq s (+ s i)))
    s)
  45)

(defun %dotimes-fixnum-count (n)
  (declare (fixnum n))
  (let ((s 0))
    (dotimes (i n) (setq s (+ s i)))
    s))

(deftest long-rep-dotimes-fixnum-count
  (%dotimes-fixnum-count 10)
  45)

;; Counter values beyond the Fixnum cache (> 65535) — boxing at use sites
;; must produce correct fresh boxes, and the native increment must not wrap.
(deftest long-rep-dotimes-large
  (%dotimes-fixnum-count 100000)
  4999950000)

;; Result form sees the final counter value (= count), boxed at the boundary.
(defun %dotimes-result-var (n)
  (declare (fixnum n))
  (dotimes (i n i)))

(deftest long-rep-dotimes-result-var
  (%dotimes-result-var 7)
  7)

(deftest long-rep-dotimes-zero
  (%dotimes-result-var 0)
  0)

;; Nested counters (2D loop shape).
(defun %dotimes-nested (n)
  (declare (fixnum n))
  (let ((s 0))
    (dotimes (i n)
      (dotimes (j n)
        (setq s (+ s (* i j)))))
    s))

(deftest long-rep-dotimes-nested
  (%dotimes-nested 4)
  36)

;; Counter captured by closures: must stay boxed; dotimes uses a single
;; binding, so all closures observe the final value.
(deftest long-rep-dotimes-captured
  (let ((fs '()))
    (dotimes (i 3) (push (lambda () i) fs))
    (mapcar #'funcall fs))
  (3 3 3))

;; Counter used in ordinary boxed contexts (list building).
(deftest long-rep-dotimes-collect
  (let ((acc '()))
    (dotimes (i 5) (push i acc))
    (nreverse acc))
  (0 1 2 3 4))

;; Non-fixnum count form: no fixnum declaration is injected; generic path.
(deftest long-rep-dotimes-generic-count
  (let ((s 0))
    (dotimes (i (car (list 4))) (setq s (+ s i)))
    s)
  6)

;;; ---- general fixnum-declared mutated locals ----

;; setq with a range-provable value: raw native store.
(deftest long-rep-setq-provable
  (let ((x 5))
    (declare (fixnum x))
    (setq x (1- x))
    (setq x (logand x 3))
    x)
  0)

;; setq with a non-provable value (full-range fixnum multiply): must route
;; through the promoting boxed path — value stays exact.
(deftest long-rep-setq-unprovable
  (let ((x 5))
    (declare (fixnum x))
    (setq x (* x 3))
    x)
  15)

;; setq value from an untyped temp local (unbox-on-store path).
(deftest long-rep-setq-from-untyped
  (let ((x 0))
    (declare (fixnum x))
    (let ((tmp (car (list 42))))
      (setq x tmp))
    x)
  42)

;; setq result is the assigned value even in value position.
(deftest long-rep-setq-value
  (let ((x 1))
    (declare (fixnum x))
    (list (setq x 9) x))
  (9 9))

;; incf/decf expand to setq on the long-rep slot.
(deftest long-rep-incf
  (let ((x 10))
    (declare (fixnum x))
    (incf x 5)
    (decf x)
    x)
  14)

;; psetq through temps.
(deftest long-rep-psetq
  (let ((a 1) (b 2))
    (declare (fixnum a b))
    (psetq a b b a)
    (list a b))
  (2 1))

;; let* sequential: later init reads an earlier long-rep sibling.
(deftest long-rep-let-star
  (let* ((a 3) (b (* a 2)))
    (declare (fixnum a b))
    (setq b (+ b a))
    b)
  9)

;; Rebinding the same name as an untyped (non-fixnum) local must shadow the
;; Int64 slot — inner references go through the ordinary boxed slot.
(deftest long-rep-shadow-untyped
  (let ((x 1))
    (declare (fixnum x))
    (setq x (1+ x))
    (let ((x (list 'a 'b)))
      (list (car x) (length x))))
  (a 2))

;; Reverse shadow: inner fixnum binding over an outer untyped name.
(deftest long-rep-shadow-typed-inner
  (let ((x (list 1 2 3)))
    (let ((x 100))
      (declare (fixnum x))
      (setq x (1- x))
      x))
  99)

;; Long-rep local as an argument to an ordinary function (boxed at call).
(deftest long-rep-call-boundary
  (let ((x 6))
    (declare (fixnum x))
    (setq x (1+ x))
    (format nil "~a" x))
  "7")

;;; ---- native (all-fixnum-param) function bodies ----

;; setq on a native body's Int64-slot parameter.
(declaim (ftype (function (fixnum) fixnum) %long-rep-native-setq))
(defun %long-rep-native-setq (n)
  (declare (fixnum n))
  (setq n (1- n))
  (if (zerop n) 0 (%long-rep-native-setq n)))

(deftest long-rep-native-param-setq
  (%long-rep-native-setq 5)
  0)

;;; ---- do loops with declared counters ----

(deftest long-rep-do-loop
  (let ((s 0))
    (do ((i 0 (1+ i)))
        ((>= i 10) s)
      (declare (fixnum i))
      (setq s (+ s i))))
  45)
