;;; A TAGBODY that no GoException can reach lowers each GO to a LEAVE straight
;;; to its tag, with no index variable and no switch. Before, every GO stored
;;; the segment index and left to a dispatch switch, which the JIT does not
;;; see as a loop; a LOOP (an exit GO and a back GO) cost several times its
;;; body.
;;;
;;; What the tests pin: the shape (no switch for a LOOP, the switch kept when a
;;; closure can GO), and the answers where the GO is not a plain statement: in
;;; an argument position with values on the stack, out of a protected region,
;;; to an outer tagbody, and back and forth between several tags.

(setf dotcl:*save-sil* t)

(defun %tdg-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %tdg-loop (n)
  (let ((acc 0)) (declare (fixnum acc))
    (loop for i below n do (setq acc (logxor acc i)))
    acc))

(defun %tdg-closure (n)
  (let ((k 0))
    (tagbody
     top
       (incf k)
       (when (< k n) (funcall (lambda () (go top)))))
    k))

(defun %tdg-arg (n)
  (let ((r '()))
    (tagbody
     top
       (push (+ 1 (if (> n 2) (go done) n)) r)
       (incf n)
       (go top)
     done)
    (nreverse r)))

(defun %tdg-protected (n)
  (let ((log '()))
    (tagbody
     top
       (unwind-protect
            (handler-case
                (progn (push n log)
                       (when (> n 2) (go out))
                       (incf n)
                       (go top))
              (error () (push :error log)))
         (push :cleanup log))
     out)
    (nreverse log)))

(defun %tdg-nested (n)
  (let ((r '()))
    (tagbody
     outer
       (tagbody
        inner
          (push n r)
          (decf n)
          (when (<= n 0) (go done))
          (when (evenp n) (go outer))
          (go inner))
     done)
    (nreverse r)))

(defun %tdg-states (s)
  (let ((r '()) (k 0))
    (tagbody
       (case s (:a (go a)) (:b (go b)) (t (go c)))
     a (push :a r) (incf k) (if (< k 4) (go b) (go c))
     b (push :b r) (incf k) (go a)
     c (push :c r))
    (nreverse r)))

(deftest-emitting-only tagbody-direct-go.shape
  (list (and (search "switch" (string-downcase (%tdg-sil #'%tdg-loop))) t)
        (and (search "switch" (string-downcase (%tdg-sil #'%tdg-closure))) t))
  (nil t))

(deftest tagbody-direct-go.values
  (list (%tdg-loop 10) (%tdg-closure 5) (%tdg-arg 0) (%tdg-protected 1)
        (%tdg-nested 5) (%tdg-states :a) (%tdg-states :b) (%tdg-states :x))
  (1 5 (1 2 3) (1 :cleanup 2 :cleanup 3 :cleanup)
   (5 4 3 2 1) (:a :b :a :b :a :c) (:b :a :b :a :c) (:c)))
