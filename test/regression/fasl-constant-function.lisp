;;; A capture-free LAMBDA / FLET / LABELS inside a .fasl is built once, not on
;;; every call of the function that contains it.
;;;
;;; In-process assembly builds the LispFunction at assembly time and loads it as a
;;; constant, so a local function that closes over nothing costs nothing per call.
;;; The fasl emitter had no equivalent: it emitted the construction, so every call
;;; allocated a fresh LispFunction, a delegate and an args-array wrapper -- 376
;;; bytes a call, measured. Everything ASDF builds is a fasl, so that was every
;;; library: cl-ppcre's scanner paid it once per SCAN.
;;;
;;; "Captures nothing" is exactly the condition under which one instance is
;;; correct, and the in-process path already produced one, so this is also a
;;; divergence between the two paths being closed. That is what the EQ test pins.

(defvar *fcf-dir*
  (let ((dir (concatenate 'string
                          (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                                  (dotcl:getenv "TEMP")
                                                  "/tmp"))
                          "/dotcl-fcf-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %fcf-compile-and-load (source name)
  (let ((lisp (concatenate 'string *fcf-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string source s))
    (load (compile-file lisp))
    t))

(defun %fcf-bytes () (nth 4 (dotcl:gc-stats)))

(defun %fcf-loop (f n arg)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (funcall f arg)))))

(defun %fcf-per-call (f arg)
  "Bytes allocated per call of F, smallest of five runs."
  (%fcf-loop f 2000 arg)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (%fcf-bytes)))
        (%fcf-loop f 100000 arg)
        (let ((used (- (%fcf-bytes) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

(deftest-compiled-only fasl-constant-function.values
  (progn
    (%fcf-compile-and-load
     "(in-package :cl-user)
      ;; capture-free FLET at the top of a DEFUN
      (defun %fcf-top (x) (flet ((f (p) (* p 2))) (+ (f x) 1)))
      ;; capture-free FLET inside a closure that does capture
      (defun %fcf-mk (k) (lambda (x) (flet ((f (p) (* p 2))) (+ (f x) k))))
      ;; capture-free LABELS, recursive
      (defun %fcf-lab (x) (labels ((g (p) (if (<= p 0) 0 (+ 1 (g (1- p)))))) (g x)))
      ;; a capture-free LAMBDA returned as a value
      (defun %fcf-thunk () (lambda (x) (* x 3)))"
     "fcf-values")
    (list (funcall (intern "%FCF-TOP") 3)
          (funcall (funcall (intern "%FCF-MK") 1) 3)
          (funcall (intern "%FCF-LAB") 4)
          (funcall (funcall (intern "%FCF-THUNK")) 5)))
  (7 7 4 15))

;;; Built once: two calls hand back the same object, as they already did for the
;;; same source compiled in-process.
(deftest-compiled-only fasl-constant-function.same-object-each-call
  (eq (funcall (intern "%FCF-THUNK")) (funcall (intern "%FCF-THUNK")))
  t)

;;; And therefore no allocation per call.
(deftest-compiled-only fasl-constant-function.allocates-nothing
  (list (= 0 (%fcf-per-call (symbol-function (intern "%FCF-TOP")) 3))
        (= 0 (%fcf-per-call (funcall (intern "%FCF-MK") 1) 3)))
  (t t))

;;; A capture-free local function still sees the arguments it is given, and a
;;; closure that DOES capture is unaffected -- it keeps being built per entry,
;;; because that is what capturing means.
(deftest-compiled-only fasl-constant-function.capturing-closure-still-fresh
  (progn
    (%fcf-compile-and-load
     "(in-package :cl-user)
      (defun %fcf-cap (k) (lambda () k))"
     "fcf-capture")
    (let ((a (funcall (intern "%FCF-CAP") 1))
          (b (funcall (intern "%FCF-CAP") 2)))
      (list (eq a b) (funcall a) (funcall b))))
  (nil 1 2))
