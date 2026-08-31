;;; FLET/LABELS capture lifting: a local function that only gets called takes its
;;; captured variables as extra arguments, which drops it onto the constant-function
;;; path and removes the per-entry closure allocation. These tests pin the semantics
;;; the transform must not change, and the cases it has to decline.

(defun %fcl-bytes ()
  (nth 4 (dotcl:gc-stats)))

;;; --- semantics: the lifted function still reads the right binding ---

(deftest flet-capture-lifting.reads-capture
  (let ((x 10))
    (flet ((add (n) (+ n x)))
      (list (add 1) (add 2))))
  (11 12))

(deftest flet-capture-lifting.labels-single
  (let ((x 100))
    (labels ((scale (n) (* n x)))
      (+ (scale 2) (scale 3))))
  500)

(deftest flet-capture-lifting.two-functions
  (let ((a 3) (b 4))
    (flet ((f (p) (+ p a))
           (g (p) (* p b)))
      (list (f 1) (g 2))))
  (4 8))

;;; An inner binding of the captured name must NOT be what the call passes: the
;;; function was written against the outer X and has to keep seeing it.
(deftest flet-capture-lifting.shadowed-at-call-site
  (let ((x 1))
    (flet ((get-x (n) (+ n x)))
      (let ((x 99))
        (declare (ignorable x))
        (get-x 0))))
  1)

(deftest flet-capture-lifting.shadowed-by-inner-let-both-calls
  (let ((x 5))
    (flet ((get-x (n) (+ n x)))
      (list (get-x 0)
            (let ((x 50)) (declare (ignorable x)) (get-x 0)))))
  (5 5))

;;; Called from inside a nested closure: the closure captured the function, not the
;;; variable, so the lift has to be abandoned rather than produce a short call.
(deftest flet-capture-lifting.called-from-closure
  (let ((x 10))
    (labels ((add-x (n) (+ n x)))
      (funcall (lambda () (add-x 5)))))
  15)

(deftest flet-capture-lifting.called-from-closure-and-directly
  (let ((x 7))
    (flet ((add-x (n) (+ n x)))
      (list (add-x 1) (funcall (lambda () (add-x 2))))))
  (8 9))

;;; Used as a value: #'f still has to be the function the user wrote.
(deftest flet-capture-lifting.function-value
  (let ((x 4))
    (flet ((add-x (n) (+ n x)))
      (mapcar #'add-x (list 1 2 3))))
  (5 6 7))

;;; A mutated capture is boxed, and its value at call time is not its value at
;;; FLET entry, so it must not be lifted.
(deftest flet-capture-lifting.mutated-capture
  (let ((acc 0))
    (flet ((bump (n) (setq acc (+ acc n)) acc))
      (list (bump 1) (bump 2) acc)))
  (1 3 3))

(deftest flet-capture-lifting.capture-mutated-outside
  (let ((x 1))
    (flet ((get-x () x))
      (let ((before (get-x)))
        (setq x 2)
        (list before (get-x)))))
  (1 2))

;;; RETURN-FROM out of the local function into an enclosing block: the block tag is
;;; a synthetic captured slot, so the lift is declined and the closure protocol
;;; re-establishes the tag.
(deftest flet-capture-lifting.return-from-enclosing-block
  (block outer
    (let ((x 3))
      (flet ((maybe (n) (when (> n 2) (return-from outer :big)) (+ n x)))
        (list (maybe 1) (maybe 5)))))
  :big)

(deftest flet-capture-lifting.return-from-own-block
  (let ((x 3))
    (flet ((f (n) (when (> n 2) (return-from f :big)) (+ n x)))
      (list (f 1) (f 5))))
  (4 :big))

;;; GO out of the local function into an enclosing tagbody: same reason.
(deftest flet-capture-lifting.go-to-enclosing-tagbody
  (let ((x 1) (hits '()))
    (tagbody
       (flet ((jump (n) (push (+ n x) hits) (go done)))
         (jump 1))
       (push :not-reached hits)
     done)
    hits)
  (2))

;;; A self-recursive LABELS function keeps its box (it has to see its own binding).
(deftest flet-capture-lifting.self-recursive-labels
  (let ((step 2))
    (labels ((countdown (n acc)
               (if (<= n 0) acc (countdown (- n step) (+ acc 1)))))
      (countdown 10 0)))
  5)

;;; Mutual recursion still works through the labels path.
(deftest flet-capture-lifting.mutual-recursion
  (let ((zero 0))
    (labels ((ev (n) (if (= n zero) t (od (- n 1))))
             (od (n) (if (= n zero) nil (ev (- n 1)))))
      (list (ev 4) (od 4))))
  (t nil))

;;; Nested FLETs, the inner one capturing the outer one's parameter.
(deftest flet-capture-lifting.nested
  (let ((base 100))
    (flet ((outer (n)
             (flet ((inner (m) (+ m n base)))
               (+ (inner 1) (inner 2)))))
      (outer 10)))
  223)

;;; The captured variable is itself a parameter of the enclosing function.
(defun %fcl-param-capture (x y)
  (flet ((combine (n) (list n x y)))
    (combine 1)))

(deftest flet-capture-lifting.captures-parameter
  (%fcl-param-capture :a :b)
  (1 :a :b))

;;; A natively-typed slot must not be lifted: passing it would box it.
(deftest flet-capture-lifting.fixnum-declared-capture
  (let ((n 0))
    (declare (fixnum n))
    (flet ((add (p) (declare (fixnum p)) (+ p n)))
      (list (add 1) (add 2))))
  (1 2))

;;; --- allocation: entering the FLET no longer builds a function object ---
;;;
;;; Compiled-only: the interpreter has no closure-construction step to remove, and
;;; an emit-free build has no compiler at all.

(defun %fcl-lift-1 (x)
  (flet ((add (n) (+ n x)))
    (add 1)))

(defun %fcl-lift-3 (x y z)
  (flet ((pick (n) (if (> n 0) x (if (< n 0) y z))))
    (pick 1)))

(defun %fcl-lift-two-fns (x)
  (flet ((f (n) (+ n x))
         (g (n) (- n x)))
    (+ (f 1) (g 2))))

(defun %fcl-lift-labels (x)
  (labels ((scale (n) (* n x)))
    (scale 2)))

(defun %fcl-loop (f n arg)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (funcall f arg)))))

(defun %fcl-per-call (f arg)
  "Bytes allocated by 100000 calls of F, as the smallest of five runs."
  (funcall #'%fcl-loop f 2000 arg)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (%fcl-bytes)))
        (%fcl-loop f 100000 arg)
        (let ((used (- (%fcl-bytes) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

(deftest-compiled-only flet-capture-lifting.allocates-nothing
  (list (= 0 (%fcl-per-call #'%fcl-lift-1 7))
        (= 0 (%fcl-per-call #'%fcl-lift-labels 7)))
  (t t))

(deftest-compiled-only flet-capture-lifting.allocates-nothing-multi
  (= 0 (%fcl-per-call #'%fcl-lift-two-fns 7))
  t)

(defun %fcl-loop3 (n a b c)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (%fcl-lift-3 a b c)))))

(deftest-compiled-only flet-capture-lifting.allocates-nothing-3-captures
  (progn
    (%fcl-loop3 2000 1 2 3)
    (let ((best nil))
      (dotimes (r 5 (= 0 best))
        (let ((before (%fcl-bytes)))
          (%fcl-loop3 100000 1 2 3)
          (let ((used (- (%fcl-bytes) before)))
            (when (or (null best) (< used best)) (setq best used)))))))
  t)

;;; --- allocation: a closure that IS built keeps its size ---
;;;
;;; The tests above pin the cases where no function object is built at all.
;;; Nothing pinned the cost of the ones that are, so a reference field added to
;;; the runtime's function object raised every closure in the image by 32 bytes
;;; and no test moved; it was found by measuring one release against the
;;; previous one. The ceiling is deliberately loose, at one capture plus a few
;;; bytes: it is here to catch another field, not to freeze a byte count.

(defun %fcl-make-adder (k)
  (lambda (n) (+ n k)))

;; %fcl-per-call returns the total for its 100000 calls; the tests above only
;; compare it against zero, where per-call and total agree.
(deftest-compiled-only flet-capture-lifting.built-closure-size-ceiling
  (<= (%fcl-per-call #'%fcl-make-adder 7) (* 255 100000))
  t)

;;; --- shapes the first cut declined for no good reason ---

;;; RETURN-FROM to the function's own block does not make a LABELS self-naming:
;;; it names the implicit block, which FLET establishes too.
(deftest flet-capture-lifting.labels-own-block-value
  (let ((x 10))
    (labels ((f (n) (when (> n 2) (return-from f :big)) (+ n x)))
      (list (f 1) (f 5))))
  (11 :big))

(deftest flet-capture-lifting.labels-own-block-three-captures
  (let ((x 1) (y 2) (z 3))
    (labels ((f (n) (when (> n 2) (return-from f :big)) (list n x y z)))
      (list (f 1) (f 5))))
  ((1 1 2 3) :big))

;;; A declaration names the function without using it as a value.
(deftest flet-capture-lifting.declared-inline
  (let ((x 5))
    (flet ((f (n) (+ n x)))
      (declare (inline f))
      (list (f 1) (f 2))))
  (6 7))

;;; Seven parameters after lifting still reaches the direct entries.
(defun %fcl-arity7 (a b c d e g)
  (flet ((f (p) (list p a b c d e g)))
    (f 1)))

(deftest flet-capture-lifting.arity-seven
  (%fcl-arity7 1 2 3 4 5 6)
  (1 1 2 3 4 5 6))

(defun %fcl-arity8 (a b c d e g h)
  (flet ((f (p) (list p a b c d e g h)))
    (f 1)))

(deftest flet-capture-lifting.arity-eight
  (%fcl-arity8 1 2 3 4 5 6 7)
  (1 1 2 3 4 5 6 7))

(defun %fcl-own-block (x)
  (labels ((f (n) (when (> n 2) (return-from f 0)) (+ n x)))
    (+ (f 1) (f 5))))

(deftest-compiled-only flet-capture-lifting.own-block-allocates-nothing
  (= 0 (%fcl-per-call #'%fcl-own-block 7))
  t)

;;; --- the arity cap is a per-call/per-entry trade, not a size limit ---
;;;
;;; Lifting saves one 248-byte closure per ENTRY into the FLET. Above arity 6 it
;;; costs an argument array per CALL (INVOKE7/INVOKE8 build one for the debugger
;;; frame), and a local function is normally called more than once per entry, so
;;; the cap sits where the per-call cost begins. These pin the VALUES on both
;;; sides of the cap: whichever way the trade is decided, the answer is the same.

(defun %fcl-a6-many (a b c d e)
  "Arity 6 after lifting -- at the cap, so this one is lifted."
  (flet ((f (p) (if (> p a) (if (> p b) c d) e)))
    (+ (+ (+ (f 1) (f 2)) (+ (f 3) (f 4)))
       (+ (+ (f 5) (f 6)) (+ (f 7) (f 8))))))

(deftest flet-capture-lifting.arity-six-many-calls
  (%fcl-a6-many 2 5 1 2 3)
  15)

(defun %fcl-a7-many (a b c d e g)
  "Arity 7 after lifting -- over the cap, so the closure stays."
  (flet ((f (p) (if (> p a) (if (> p b) c d) (if (> p e) g 0))))
    (+ (+ (+ (f 1) (f 2)) (+ (f 3) (f 4)))
       (+ (+ (f 5) (f 6)) (+ (f 7) (f 8))))))

(deftest flet-capture-lifting.arity-seven-many-calls
  (%fcl-a7-many 2 5 1 2 3 4)
  9)

;;; Arity 7 and 8 are inside the cap once INVOKE7/INVOKE8 stop building the
;;; debugger frame array for an anonymous callee. Measured against a baseline of
;;; the SAME outer arity and no FLET at all, because a named function of 5-8
;;; parameters pushes a frame array of its own on every call -- that cost belongs
;;; to the caller, not to the lift, and comparing against 0 would measure it.

(defun %fcl-a7-body (a b c d e g)
  (flet ((f (p) (if (> p a) (if (> p b) c d) (if (> p e) g 0))))
    (+ (+ (+ (f 1) (f 2)) (+ (f 3) (f 4)))
       (+ (+ (f 5) (f 6)) (+ (f 7) (f 8))))))

(defun %fcl-a7-baseline (a b c d e g)
  (declare (ignore b c d e g))
  a)

(defun %fcl-loop6 (f n a b c d e g)
  (declare (fixnum n))
  (let ((r 0))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (funcall f a b c d e g)))))

(defun %fcl-per-call6 (f)
  (%fcl-loop6 f 2000 2 5 1 2 3 4)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (%fcl-bytes)))
        (%fcl-loop6 f 100000 2 5 1 2 3 4)
        (let ((used (- (%fcl-bytes) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

(deftest-compiled-only flet-capture-lifting.arity-seven-costs-nothing-extra
  ;; Within 1 byte per call of the baseline. Not equality: the two runs differ by
  ;; a rounding's worth. The regression this guards against was 80 B per call.
  (<= (%fcl-per-call6 (function %fcl-a7-body))
      (+ (%fcl-per-call6 (function %fcl-a7-baseline)) 100000))
  t)

;;; --- lifting keeps symbol-identity scoping ---
;;;
;;; Captured variables become parameters under their own names, so two distinct
;;; variables that PRINT the same both end up as parameters called X. That is
;;; fine: every lookup resolves by symbol identity first and the parameter slots
;;; are keyed by the real symbols. These pin it, and they are also the reason the
;;; capture list must NOT be deduplicated by name -- these are two variables.

(defpackage :dotcl-fcl-a (:use))
(defpackage :dotcl-fcl-b (:use))

(defun %fcl-two-same-named ()
  (let ((dotcl-fcl-a::x 1))
    (let ((dotcl-fcl-b::x 2))
      (flet ((f (n) (list n dotcl-fcl-a::x dotcl-fcl-b::x)))
        (list (f 0) (f 9))))))

(deftest flet-capture-lifting.two-same-named-captures
  (%fcl-two-same-named)
  ((0 1 2) (9 1 2)))

(defun %fcl-two-same-named-plus (k)
  (let ((dotcl-fcl-a::y 10))
    (let ((dotcl-fcl-b::y 20))
      (flet ((g (n) (list n dotcl-fcl-a::y dotcl-fcl-b::y k)))
        (list (g 1) (g 2))))))

(deftest flet-capture-lifting.two-same-named-plus-outer
  (%fcl-two-same-named-plus :k)
  ((1 10 20 :k) (2 10 20 :k)))

;;; --- block tags travel as arguments too ---
;;;
;;; A local function that RETURN-FROMs an enclosing NAMED block closes over that
;;; block's tag, which is a compiler-synthesized slot. Passing the tag in as an
;;; argument lets the function be lifted like any other, and COMPILE-FUNCTION-
;;; BODY-DIRECT rebuilds the block table from the parameter it arrives in.
;;;
;;; BLOCK NIL is excluded: every LOOP, DO and DOLIST establishes one, so several
;;; are in scope at once and the innermost is not the one the local function was
;;; written against. The shapes below pin both sides of that line.

(defun %fcl-outer-return (x)
  (block scan
    (labels ((adv (n) (when (> n 2) (return-from scan :big)) (+ n x)))
      (list (adv 1) (adv 5)))))

(deftest flet-capture-lifting.return-from-enclosing-named-block
  (list (%fcl-outer-return 10) (%fcl-outer-return 0))
  (:big :big))

(defun %fcl-outer-return-small (x)
  (block scan
    (labels ((adv (n) (when (> n 2) (return-from scan :big)) (+ n x)))
      (list (adv 1) (adv 2)))))

(deftest flet-capture-lifting.enclosing-block-not-taken
  (%fcl-outer-return-small 10)
  (11 12))

;;; Two enclosing blocks of the same name: the inner one is what RETURN-FROM
;;; means, and the lift must not reach past it.
(defun %fcl-shadowed-block (x)
  (block scan
    (list :outer
          (block scan
            (flet ((adv (n) (when (> n 2) (return-from scan :inner)) (+ n x)))
              (list (adv 1) (adv 5)))))))

(deftest flet-capture-lifting.shadowed-enclosing-block
  (%fcl-shadowed-block 10)
  (:outer :inner))

;;; BLOCK NIL from inside a LOOP: (return ...) leaves the loop the local function
;;; was written in, not the inner one it happens to be called from.
(defun %fcl-loop-return (items)
  (loop :with acc = 0 :for s :in items :do
    (flet ((consider (i) (cond ((= i 1) (return i)))))
      (cond ((characterp s) (consider 0))
            ((stringp s) (loop :for c :across s
                               :do (consider (if (char= c #\z) 1 0))))))
        :finally (return acc)))

(deftest flet-capture-lifting.block-nil-from-loop
  (list (%fcl-loop-return (list #\a))
        (%fcl-loop-return (list "az"))
        (%fcl-loop-return (list "ab")))
  (0 1 0))

;;; GO out of a lifted candidate into an enclosing TAGBODY keeps working: the
;;; tagbody id is a synthetic slot that is NOT lifted, so the closure stays.
(defun %fcl-go-out (x)
  (let ((hits '()))
    (tagbody
       (flet ((jump (n) (push (+ n x) hits) (go done)))
         (jump 1))
       (push :not-reached hits)
     done)
    hits))

(deftest flet-capture-lifting.go-out-still-works
  (%fcl-go-out 1)
  (2))

(defun %fcl-outer-return-loop (n x)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (%fcl-outer-return-small x)))))

;;; The point of all this: entering the LABELS no longer builds a function.
;;; What is left is the block tag itself, which a non-local exit needs, plus the
;;; two conses the body makes -- so this is measured against the same shape with
;;; the RETURN-FROM removed, not against zero.
(defun %fcl-outer-noreturn-small (x)
  (block scan
    (labels ((adv (n) (+ n x)))
      (list (adv 1) (adv 2)))))

(defun %fcl-outer-noreturn-loop (n x)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (%fcl-outer-noreturn-small x)))))

(deftest-compiled-only flet-capture-lifting.enclosing-block-lift-allocates-little
  ;; Within one cons (32 bytes) per call of the RETURN-FROM-free shape. Before
  ;; the tag could be lifted this was a whole closure more, 280 bytes.
  (let ((with (progn (%fcl-outer-return-loop 2000 10)
                     (let ((best nil))
                       (dotimes (r 5 best)
                         (let ((b (%fcl-bytes)))
                           (%fcl-outer-return-loop 100000 10)
                           (let ((u (- (%fcl-bytes) b)))
                             (when (or (null best) (< u best)) (setq best u))))))))
        (without (progn (%fcl-outer-noreturn-loop 2000 10)
                        (let ((best nil))
                          (dotimes (r 5 best)
                            (let ((b (%fcl-bytes)))
                              (%fcl-outer-noreturn-loop 100000 10)
                              (let ((u (- (%fcl-bytes) b)))
                                (when (or (null best) (< u best)) (setq best u)))))))))
    (<= with (+ without 4000000)))
  t)
