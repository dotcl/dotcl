;;; bench/micro-gf-dispatch.lisp -- what one generic-function call allocates
;;;
;;; Usage (from the project root, ALWAYS Release):
;;;   dotnet run -c Release --project runtime/runtime.csproj -- \
;;;     --asm compiler/cil-out.sil bench/micro-gf-dispatch.lisp
;;;
;;; Allocation, not time: on this class of machine a single call's time is well
;;; inside the +-30% noise floor, while the bytes are exact and repeat to the
;;; tenth. Read a row as "B/call including the loop baseline" and compare rows,
;;; or subtract the baseline row for the bare cost.
;;;
;;; What the rows are for:
;;;   plain defun   the floor -- a compiled call with a direct entry allocates
;;;                 nothing, so anything above the baseline row is dispatch.
;;;   gf 1/2/3/4-arg  the slope is the argument array (8 B a slot); the intercept
;;;                 is what dispatch itself costs. A closure capturing the
;;;                 dispatcher's locals used to sit in that intercept (56 B on
;;;                 every call, including cache hits that never used it).
;;;   accessor      a slot reader served by the call-site inline cache, which
;;;                 bypasses dispatch entirely.
;;;   gf 0-arg      a generic function with nothing to dispatch on. It has no
;;;                 cache key, so it re-computes the applicable methods every
;;;                 call -- an order of magnitude above the others, and the
;;;                 reason this row is here rather than in with its siblings.

(defmacro bytes-per (n label &body body)
  `(let ((c0 (nth 4 (dotcl:gc-stats))))
     (progn ,@body)
     (format t "~A ~,1F B/call~%" ,label (/ (- (nth 4 (dotcl:gc-stats)) c0) (float ,n)))))

(defclass gfb () ((s :initform 42 :accessor gfb-s)))
(defvar *o* (make-instance 'gfb))

(defgeneric g0 ())
(defmethod g0 () 1)
(defgeneric g1 (a))
(defmethod g1 ((a gfb)) 1)
(defgeneric g2 (a b))
(defmethod g2 ((a gfb) b) 1)
(defgeneric g3 (a b c))
(defmethod g3 ((a gfb) b c) 1)
;; One :after method around a single primary. The loose-argument dispatch path
;; used to run only the "one primary, nothing else" shape and hand every other
;; cache hit to the array path, so this row sat 32 B above its plain twin --
;; on every call, warm cache included.
(defgeneric g4 (a b c d))
(defmethod g4 ((a gfb) b c d) 1)
(defgeneric ga (a))
(defmethod ga ((a gfb)) 1)
(defmethod ga :after ((a gfb)) nil)
(defun plain1 (a) a)

(defvar *n* 200000)

;; Warm the dispatch caches and let tiered JIT settle before measuring.
(dotimes (i 2000) (g0) (g1 *o*) (g2 *o* 1) (g3 *o* 1 2) (g4 *o* 1 2 3) (ga *o*)
                  (plain1 *o*) (gfb-s *o*))

(bytes-per *n* "loop baseline    " (dotimes (i *n*) nil))
(bytes-per *n* "plain defun 1-arg" (dotimes (i *n*) (plain1 *o*)))
(bytes-per *n* "accessor (IC)    " (dotimes (i *n*) (gfb-s *o*)))
(bytes-per *n* "gf 1-arg         " (dotimes (i *n*) (g1 *o*)))
(bytes-per *n* "gf 2-arg         " (dotimes (i *n*) (g2 *o* 1)))
(bytes-per *n* "gf 3-arg         " (dotimes (i *n*) (g3 *o* 1 2)))
(bytes-per *n* "gf 4-arg         " (dotimes (i *n*) (g4 *o* 1 2 3)))
(bytes-per *n* "gf 1-arg +:after " (dotimes (i *n*) (ga *o*)))
(bytes-per *n* "gf 0-arg         " (dotimes (i *n*) (g0)))
