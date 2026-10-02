;;; A &key function keeps its body once: the array entry and the typed entries
;;; (required arguments only, one keyword pair) all call one shared body that
;;; takes every key as a positional argument, with a marker for a key the call
;;; did not supply.
;;;
;;; Each entry used to carry its own copy of the body, and each is JIT-compiled
;;; on its first call, so a function called both with and without keywords paid
;;; the JIT for its body two or three times. These check that every behaviour
;;; of the separate copies survived, that tail self-calls still loop, and that
;;; the copies are really gone.

(defun %ksb-compile-string (form)
  (let ((compile-toplevel (find-symbol "COMPILE-TOPLEVEL" "DOTCL.CIL-COMPILER")))
    (prin1-to-string (funcall compile-toplevel form))))

(defun %ksb-count (needle haystack)
  (loop with start = 0
        for pos = (search needle haystack :start2 start)
        while pos
        count t
        do (setf start (1+ pos))))

;; Defaults read earlier parameters, a supplied-p var, several keys.
(defun %ksb-f (a &key (b 1) (c (+ a b)) (d nil dp))
  (list a b c d dp))

(deftest key-shared-body.no-keywords-takes-defaults
  (%ksb-f 10)
  (10 1 11 nil nil))

(deftest key-shared-body.one-keyword
  (list (%ksb-f 10 :b 5) (%ksb-f 10 :d 7) (%ksb-f 10 :c 0) (%ksb-f 10 :d nil))
  ((10 5 15 nil nil) (10 1 11 7 t) (10 1 0 nil nil) (10 1 11 nil t)))

(deftest key-shared-body.several-keywords
  (list (%ksb-f 10 :c 3 :b 2) (%ksb-f 10 :b 2 :b 99) (%ksb-f 10 :d 1 :b 2 :c 3))
  ((10 2 3 nil nil) (10 2 12 nil nil) (10 2 3 1 t)))

(deftest key-shared-body.allow-other-keys-pair
  (list (%ksb-f 10 :allow-other-keys nil) (%ksb-f 10 :zz 1 :allow-other-keys t))
  ((10 1 11 nil nil) (10 1 11 nil nil)))

(deftest key-shared-body.unknown-keyword-still-signals
  (list (handler-case (progn (%ksb-f 10 :zz 1) :no-error)
          (program-error () :error))
        (handler-case (progn (%ksb-f 10 :b 1 :zz 1) :no-error)
          (program-error () :error)))
  (:error :error))

(deftest key-shared-body.apply-and-funcall
  (list (funcall #'%ksb-f 1) (apply #'%ksb-f 1 '(:b 2)) (apply #'%ksb-f '(1))
        (apply #'%ksb-f 1 '(:c 5 :b 2)))
  ((1 1 2 nil nil) (1 2 3 nil nil) (1 1 2 nil nil) (1 2 5 nil nil)))

;; A default with a side effect runs exactly once, and only when the key is
;; absent.
(defvar *ksb-count* 0)
(defun %ksb-g (x &key (y (incf *ksb-count*))) (list x y))

(deftest key-shared-body.default-evaluated-once
  (progn (setq *ksb-count* 0)
         (list (%ksb-g 1) *ksb-count* (%ksb-g 1 :y 9) *ksb-count*
               (apply #'%ksb-g 1 nil) *ksb-count*))
  ((1 1) 1 (1 9) 1 (1 2) 2))

;; A key that is itself named ALLOW-OTHER-KEYS.
(defun %ksb-h (x &key allow-other-keys) (list x allow-other-keys))

(deftest key-shared-body.key-named-allow-other-keys
  (list (%ksb-h 1) (%ksb-h 1 :allow-other-keys 5))
  ((1 nil) (1 5)))

(defun %ksb-aok (x &key y &allow-other-keys) (list x y))

(deftest key-shared-body.allow-other-keys-lambda-list
  (list (%ksb-aok 1) (%ksb-aok 1 :q 2) (%ksb-aok 1 :y 3) (%ksb-aok 1 :q 1 :y 3))
  ((1 nil) (1 nil) (1 3) (1 3)))

;; Recursion through every shape, including a non-tail self-call (such a body
;; is not shared, and keeps its copies).
(defun %ksb-r (n &key (acc nil))
  (if (zerop n) acc (cons n (%ksb-r (1- n) :acc acc))))
(defun %ksb-s (n &key (tag :t)) (if (zerop n) (list tag) (cons n (%ksb-s (1- n)))))

(deftest key-shared-body.non-tail-recursion
  (list (%ksb-r 3) (%ksb-r 2 :acc '(x)) (%ksb-s 2) (%ksb-s 1 :tag :u))
  ((3 2 1) (2 1 x) (2 1 :t) (1 :t)))

;; A tail self-call loops, with or without keywords: a million deep would
;; exhaust the stack otherwise. Without keywords the defaults are taken again
;; on each iteration, as a real call would.
(defun %ksb-loop (n acc &key (step 1))
  (if (<= n 0) acc (%ksb-loop (- n step) (+ acc 1))))

(defun %ksb-loop-kw (n acc &key (step 1) (bump 1))
  (if (<= n 0) acc (%ksb-loop-kw (- n step) (+ acc bump) :step step :bump bump)))

(defvar *ksb-defaults* 0)
(defun %ksb-loop-default (n &key (seen (incf *ksb-defaults*)))
  (if (<= n 0) seen (%ksb-loop-default (1- n))))

(deftest-compiled-only key-shared-body.tail-self-call-loops
  (list (%ksb-loop 1000000 0) (%ksb-loop 1000000 0 :step 2))
  (1000000 999999))

(deftest-compiled-only key-shared-body.tail-self-call-with-keywords-loops
  (%ksb-loop-kw 1000000 0 :step 1 :bump 2)
  2000000)

(deftest key-shared-body.tail-self-call-retakes-defaults
  (progn (setq *ksb-defaults* 0)
         (list (%ksb-loop-default 3) *ksb-defaults*
               (%ksb-loop-default 2 :seen :given) *ksb-defaults*))
  (4 4 6 6))

;; Keywords out of declaration order in a tail self-call: the arguments must
;; still be evaluated in the order written.
(defvar *ksb-log* nil)
(defun %ksb-order (n &key (a 0) (b 0))
  (if (zerop n)
      (list a b)
      (%ksb-order (1- n) :b (progn (push :b *ksb-log*) (1+ b))
                         :a (progn (push :a *ksb-log*) (1+ a)))))

(deftest key-shared-body.keyword-order-kept
  (progn (setq *ksb-log* nil)
         (list (%ksb-order 2) (reverse *ksb-log*)))
  ((2 2) (:b :a :b :a)))

;; A closure over a key variable sees the binding of its own iteration.
(defun %ksb-closures (n fns &key (tag n))
  (if (zerop n) (mapcar #'funcall fns) (%ksb-closures (1- n) (cons (lambda () tag) fns))))

(deftest key-shared-body.closure-per-iteration
  (%ksb-closures 3 nil)
  (1 2 3))

;; A RETURN-FROM the function keeps its block.
(defun %ksb-ret (x &key (limit 10))
  (dolist (y x) (when (> y limit) (return-from %ksb-ret y)))
  :none)

(deftest key-shared-body.return-from
  (list (%ksb-ret '(1 20 3)) (%ksb-ret '(1 20 3) :limit 30) (%ksb-ret '(5) :limit 1))
  (20 :none 5))

;; Multiple values come back through every entry.
(defun %ksb-mv (x &key (y 2)) (values x y))

(deftest key-shared-body.multiple-values
  (list (multiple-value-list (%ksb-mv 1)) (multiple-value-list (%ksb-mv 1 :y 3))
        (multiple-value-list (apply #'%ksb-mv 1 '(:y 4))))
  ((1 2) (1 3) (1 4)))

;; The body is compiled once for the fasl backend: the shared body carries it,
;; and the array entry and both typed entries are calls to it. The one other
;; copy is the required-only shape's body for the in-memory backend, which
;; cannot inline the call and so keeps a copy of its own (the fasl ignores it).
(defun %ksb-sink (x) x)

(deftest-emitting-only key-shared-body.body-compiled-once
  (let ((s (%ksb-compile-string
            `(defun %ksb-probe (a &key (b 1))
               (%ksb-sink :marker-for-count)
               ,@(loop repeat 60 collect '(%ksb-sink a))
               (list a b)))))
    (list (and (search ":SHARED" s) t)
          (%ksb-count "CALL-KEY-SHARED" s)
          (%ksb-count "MARKER-FOR-COUNT" s)))
  (t 3 2))

;; A small body keeps its own copy in the required-only entry: the JIT of a
;; copy is cheap, and the copy folds a constant default. The other two entries
;; still call the shared body.
(deftest-emitting-only key-shared-body.small-body-keeps-required-only-copy
  (let ((s (%ksb-compile-string
            '(defun %ksb-probe-small (a &key (b 1))
              (list :marker-for-count a b)))))
    (list (%ksb-count "CALL-KEY-SHARED" s)
          (%ksb-count "MARKER-FOR-COUNT" s)))
  (2 2))
