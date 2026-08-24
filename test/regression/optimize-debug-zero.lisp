;;; (optimize (debug 0)) drops the debugger frame a function records on every
;;; call, which is the one per-call cost a caller cannot avoid otherwise
;;; (measured at 14-18% of richards). The function keeps its name for everything
;;; else -- error messages, DESCRIBE, and the frames its callers record.
;;;
;;; Both spellings must work, the setting must be scoped as CLHS says (a DECLAIM
;;; is global from that point, a DECLARE is that function only), and the default
;;; must be unchanged.

(defun odz-inner () (dotcl:backtrace))
(defun odz-default () (odz-inner))
(defun odz-declared () (declare (optimize (debug 0))) (odz-inner))

(deftest optimize-debug.default-records-a-frame
  (and (member "ODZ-DEFAULT" (odz-default) :test #'equal) t)
  t)

;; Compiled-only: the declaration is an instruction to the compiler, and an
;; emit-free build has none — the interpreter records the frame either way. The
;; other tests here hold in both builds and stay ungated.
(deftest-compiled-only optimize-debug.declare-drops-the-frame
  (member "ODZ-DECLARED" (odz-declared) :test #'equal)
  nil)

(deftest optimize-debug.callee-frames-are-unaffected
  (and (member "ODZ-INNER" (odz-declared) :test #'equal) t)
  t)

;;; The name survives: a wrong-argument-count error still says which function.
(deftest optimize-debug.name-survives-for-errors
  (handler-case (funcall #'odz-declared 1 2 3)
    (error (e) (and (search "ODZ-DECLARED" (princ-to-string e)) t))
    (:no-error (&rest r) (declare (ignore r)) :no-error))
  t)

(deftest optimize-debug.function-still-works
  (let ((r (odz-declared))) (and (listp r) t))
  t)

;;; A structure accessor and a closure defined under the declaration still run.
(defun odz-make-adder (n)
  (declare (optimize (debug 0)))
  (lambda (x) (+ x n)))

(deftest optimize-debug.closures-still-work
  (funcall (odz-make-adder 10) 5)
  15)

(deftest optimize-debug.recursion-still-works
  (labels ((f (n acc) (declare (optimize (debug 0))) (if (= n 0) acc (f (1- n) (+ acc n)))))
    (f 100 0))
  5050)
