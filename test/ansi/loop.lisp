;;; loop.lisp — ANSI tests for the LOOP macro

;;; ============================================================
;;; Simple loop
;;; ============================================================

(deftest loop.simple.1
  (let ((n 0))
    (loop
      (setq n (+ n 1))
      (if (= n 5) (return n))))
  5)

;;; ============================================================
;;; FOR ... IN
;;; ============================================================

(deftest loop.for-in.1
  (loop for x in '(1 2 3) collect x)
  (1 2 3))

(deftest loop.for-in.2
  (loop for x in '(1 2 3) collect (* x x))
  (1 4 9))

(deftest loop.for-in.empty
  (loop for x in nil collect x)
  nil)

;;; ============================================================
;;; FOR ... FROM / TO / BELOW / BY
;;; ============================================================

(deftest loop.for-from.1
  (loop for i from 0 below 5 collect i)
  (0 1 2 3 4))

(deftest loop.for-from.2
  (loop for i from 1 to 5 collect i)
  (1 2 3 4 5))

(deftest loop.for-from.by
  (loop for i from 0 below 10 by 3 collect i)
  (0 3 6 9))

(deftest loop.for-below-only
  (loop for i below 4 collect i)
  (0 1 2 3))

;;; ============================================================
;;; FOR ... = [THEN]
;;; ============================================================

(deftest loop.for-eq.1
  (loop for x in '(1 2 3)
        for y = (* x x)
        collect y)
  (1 4 9))

(deftest loop.for-eq-then.1
  (loop for x = 1 then (* x 2)
        repeat 5
        collect x)
  (1 2 4 8 16))

;;; ============================================================
;;; FOR ... ON
;;; ============================================================

(deftest loop.for-on.1
  (loop for x on '(1 2 3) collect x)
  ((1 2 3) (2 3) (3)))

;;; ============================================================
;;; FOR ... ACROSS
;;; ============================================================

(deftest loop.for-across.1
  (loop for c across "abc" collect c)
  (#\a #\b #\c))

;;; ============================================================
;;; DO
;;; ============================================================

(deftest loop.do.1
  (let ((sum 0))
    (loop for x in '(1 2 3) do (setq sum (+ sum x)))
    sum)
  6)

;;; ============================================================
;;; COLLECT
;;; ============================================================

(deftest loop.collect.1
  (loop for i from 1 to 3 collect (* i 10))
  (10 20 30))

;;; ============================================================
;;; APPEND
;;; ============================================================

(deftest loop.append.1
  (loop for x in '((1 2) (3 4) (5)) append x)
  (1 2 3 4 5))

;;; ============================================================
;;; SUM
;;; ============================================================

(deftest loop.sum.1
  (loop for x in '(1 2 3 4 5) sum x)
  15)

;;; ============================================================
;;; COUNT
;;; ============================================================

(deftest loop.count.1
  (loop for x in '(1 2 3 4 5) count (oddp x))
  3)

;;; ============================================================
;;; WHEN / UNLESS
;;; ============================================================

(deftest loop.when.1
  (loop for x in '(1 2 3 4 5) when (oddp x) collect x)
  (1 3 5))

(deftest loop.unless.1
  (loop for x in '(1 2 3 4 5) unless (oddp x) collect x)
  (2 4))

(deftest loop.when-else.1
  (loop for x in '(1 2 3 4 5)
        when (oddp x) collect x
        else collect (- 0 x))
  (1 -2 3 -4 5))

;;; ============================================================
;;; WHILE / UNTIL
;;; ============================================================

(deftest loop.while.1
  (loop for x in '(1 2 3 4 5) while (< x 4) collect x)
  (1 2 3))

(deftest loop.until.1
  (loop for x in '(1 2 3 4 5) until (> x 3) collect x)
  (1 2 3))

;;; ============================================================
;;; WITH
;;; ============================================================

(deftest loop.with.1
  (loop with acc = 0
        for x in '(1 2 3)
        do (setq acc (+ acc x))
        finally (return acc))
  6)

;;; ============================================================
;;; REPEAT
;;; ============================================================

(deftest loop.repeat.1
  (loop repeat 3 collect 1)
  (1 1 1))

(deftest loop.repeat.0
  (loop repeat 0 collect 1)
  nil)

;;; ============================================================
;;; ALWAYS / NEVER / THEREIS
;;; ============================================================

(deftest loop.always.true
  (loop for x in '(1 2 3) always (numberp x))
  t)

(deftest loop.always.false
  (loop for x in '(1 2 "a") always (numberp x))
  nil)

(deftest loop.thereis.1
  (loop for x in '(1 2 3 4 5) thereis (if (> x 3) x nil))
  4)

(deftest loop.thereis.nil
  (loop for x in '(1 2 3) thereis (if (> x 10) x nil))
  nil)

;;; ============================================================
;;; FINALLY
;;; ============================================================

(deftest loop.finally.1
  (loop for x in '(1 2 3)
        do (+ x 1)
        finally (return 42))
  42)

;;; ============================================================
;;; RETURN
;;; ============================================================

(deftest loop.return.1
  (loop for x in '(1 2 3 4 5)
        when (= x 3) return x)
  3)

;;; ============================================================
;;; Parallel FOR clauses
;;; ============================================================

(deftest loop.parallel.1
  (loop for x in '(a b c)
        for i from 1
        collect (list i x))
  ((1 a) (2 b) (3 c)))

(deftest loop.parallel.shorter
  ;; Parallel for: ends when shortest exhausted
  (loop for x in '(1 2 3 4 5)
        for y in '(a b c)
        collect (list x y))
  ((1 a) (2 b) (3 c)))

;;; ============================================================
;;; Destructuring FOR
;;; ============================================================

(deftest loop.destr-for-in.1
  (loop for (a . b) in '((1 . 2) (3 . 4)) collect (+ a b))
  (3 7))

(deftest loop.destr-for-in.2
  (loop for (a b) in '((1 2) (3 4) (5 6)) collect (+ a b))
  (3 7 11))

;;; ============================================================
;;; Keyword-style loop (ASDF uses :for :in etc.)
;;; ============================================================

(deftest loop.kw-style.1
  (loop :for x :in '(1 2 3) :collect (* x 2))
  (2 4 6))

(deftest loop.kw-style.2
  (loop :for i :from 0 :below 3 :collect i)
  (0 1 2))

;;; ============================================================
;;; Multiple body actions
;;; ============================================================

(deftest loop.multi-action.1
  (let ((side nil))
    (list
      (loop for x in '(1 2 3)
            when (oddp x) collect x
            and do (push x side))
      (nreverse side)))
  ((1 3) (1 3)))

(do-tests-summary)
