;;; A LABELS function that captures something is still a loop.
;;;
;;; The named-loop idiom -- a local function that closes over the enclosing
;;; variables and calls itself in tail position -- used to keep a frame per
;;; iteration and overflow the stack, while the same loop written with every
;;; value passed as an argument did not. The difference was the capture: a
;;; capturing local function is compiled as a closure, and the closure boundary
;;; deliberately clears the self-TCO handoff (a closure is normally not the
;;; self-call target of the body it appears in). A labels function is the
;;; exception -- it IS its own call target.
;;;
;;; The iteration counts are what make these tests mean anything: the deep ones
;;; are far past the stack, so a lost loop is an overflow, not a slow pass. Those
;;; are DEFTEST-COMPILED-ONLY -- the tree-walk interpreter has no TCO of its own,
;;; so depth there says nothing about the code this file is about.

;;; (a) The reported case: one captured variable, tail self-call.
(defun %lct-sum (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((lp (i s)
             (declare (fixnum i s))
             (if (>= i n) s (lp (1+ i) (+ s (logand i 255))))))
    (lp 0 0)))

(deftest labels-closure-tco.captured-bound
  (%lct-sum 100000)
  12742320)

(deftest-compiled-only labels-closure-tco.deep
  (%lct-sum 10000000)
  1274991808)

;;; (b) Two captured variables, and one of them is read on every iteration.
(defun %lct-two-captures (n step)
  (declare (fixnum n step))
  (labels ((lp (i acc)
             (declare (fixnum i acc))
             (if (>= i n) acc (lp (+ i step) (+ acc step)))))
    (lp 0 0)))

(deftest-compiled-only labels-closure-tco.two-captures
  (%lct-two-captures 2000000 2)
  2000000)

;;; (c) Mutual recursion between two capturing local functions. Neither is a
;;; self-call, so this exercises the path the self-TCO loop must not break.
(defun %lct-parity (n)
  (declare (fixnum n))
  (let ((limit n))
    (labels ((evn (i) (if (>= i limit) :even (odd (1+ i))))
             (odd (i) (if (>= i limit) :odd (evn (1+ i)))))
      (evn 0))))

(deftest-compiled-only labels-closure-tco.mutual
  (list (%lct-parity 1000000) (%lct-parity 999999))
  (:even :odd))

;;; (d) A NON-tail self-call must stay a call: the loop may not swallow it, or
;;; the addition after the recursive call would never happen.
(defun %lct-non-tail (n)
  (declare (fixnum n))
  (labels ((lp (i)
             (if (>= i n) 0 (+ 1 (lp (1+ i))))))
    (lp 0)))

(deftest labels-closure-tco.non-tail-still-recurses
  (%lct-non-tail 1000)
  1000)

;;; A mixed body: the tail arm loops, the non-tail arm recurses, same function.
(defun %lct-mixed (n)
  (declare (fixnum n))
  (labels ((lp (i acc)
             (cond ((>= i n) acc)
                   ((= i 0) (+ 0 (lp 1 acc)))
                   (t (lp (1+ i) (+ acc 1))))))
    (lp 0 0)))

(deftest-compiled-only labels-closure-tco.mixed-tail-and-non-tail
  (%lct-mixed 500000)
  499999)

;;; (e) The local function ASSIGNS a captured variable, so that variable is
;;; boxed. The loop writes through the box; the value after it has to be the
;;; one the last iteration stored.
(defun %lct-setq-capture (n)
  (declare (fixnum n))
  (let ((total 0)
        (last -1))
    (declare (fixnum total last))
    (labels ((lp (i)
               (declare (fixnum i))
               (if (>= i n)
                   total
                   (progn (setq total (+ total i))
                          (setq last i)
                          (lp (1+ i))))))
      (list (lp 0) last))))

(deftest-compiled-only labels-closure-tco.setq-captured
  (%lct-setq-capture 1000000)
  (499999500000 999999))

;;; A boxed PARAMETER: the parameter is captured by an inner lambda, so it lives
;;; in a box, and the loop has to rewrite the box rather than the local.
(defun %lct-boxed-param (n)
  (declare (fixnum n))
  (labels ((lp (i acc)
             (declare (fixnum i))
             (let ((peek (lambda () i)))
               (if (>= i n)
                   acc
                   (lp (1+ i) (+ acc (funcall peek)))))))
    (lp 0 0)))

(deftest-compiled-only labels-closure-tco.boxed-param
  (%lct-boxed-param 200000)
  19999900000)

;;; The captured variable must be read afresh on every iteration, not frozen at
;;; the value it had when the closure was built.
(defun %lct-reads-capture (n)
  (let ((stop n))
    (labels ((lp (i)
               (if (>= i stop)
                   i
                   (progn (setq stop (min stop (+ i 10)))
                          (lp (1+ i))))))
      (lp 0))))

(deftest labels-closure-tco.capture-is-live
  (%lct-reads-capture 1000000)
  10)

;;; An inner lambda that calls the labels function must not be turned into the
;;; loop itself: it has its own parameter list, and branching to its top would
;;; run the wrong body.
(defun %lct-inner-lambda (n)
  (declare (fixnum n))
  (labels ((lp (i acc)
             (declare (fixnum i acc))
             (if (>= i n)
                 acc
                 (funcall (lambda (j) (lp (1+ j) (+ acc 1))) i))))
    (lp 0 0)))

(deftest labels-closure-tco.inner-lambda-calls-out
  (%lct-inner-lambda 10000)
  10000)

;;; A local function whose parameter is declared special cannot become a loop
;;; (the dynamic binding has to be popped), and must still return the right
;;; value.
(defun %lct-special-param (n)
  (labels ((lp (i acc)
             (declare (special i))
             (if (>= i n) acc (lp (1+ i) (+ acc (symbol-value 'i))))))
    (lp 0 0)))

(deftest labels-closure-tco.special-param
  (%lct-special-param 100)
  4950)
