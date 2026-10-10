;;; A closure made by code loaded from a .fasl shares its body delegate with the
;;; other closures of the same code site, as a closure compiled in-process does.
;;;
;;; The fasl emitter built a fresh delegate (64 bytes) for every closure it made,
;;; on top of the function object and the captured-variable array. Everything ASDF
;;; builds is a fasl, so that was every library closure: Coalton's type checker
;;; makes them by the million.

(defvar *fcd-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-fcd-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %fcd-compile-and-load (source name)
  (let ((lisp (concatenate 'string *fcd-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string source s))
    (load (compile-file lisp))
    t))

(defun %fcd-bytes () (nth 4 (dotcl:gc-stats)))

(defun %fcd-loop (f n arg)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (funcall f arg)))))

(defun %fcd-per-call (f arg)
  "Bytes allocated per call of F, smallest of five runs."
  (%fcd-loop f 2000 arg)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (%fcd-bytes)))
        (%fcd-loop f 100000 arg)
        (let ((used (- (%fcd-bytes) before)))
          (when (or (null best) (< used best)) (setq best (floor used 100000))))))))

(defvar *fcd-source*
  "(in-package :cl-user)
   (defun %fcd-mk1 (k) (lambda (x) (+ x k)))
   (defun %fcd-mk2 (k) (lambda (x y) (list x y k)))
   (defun %fcd-mkrest (k) (lambda (&rest xs) (cons k xs)))")

(deftest-compiled-only fasl-closure-delegate.values
  (progn
    (%fcd-compile-and-load *fcd-source* "fcd")
    (let ((a (funcall (intern "%FCD-MK1") 1))
          (b (funcall (intern "%FCD-MK1") 2)))
      (list (funcall a 10) (funcall b 10) (eq a b)
            (funcall (funcall (intern "%FCD-MK2") 3) 1 2)
            (apply (funcall (intern "%FCD-MK2") 3) '(4 5))
            (funcall (funcall (intern "%FCD-MKREST") 0) 1 2))))
  (11 12 nil (1 2 3) (4 5 3) (0 1 2)))

;;; The same closures compiled in-process cost the same per closure (within a few
;;; bytes of measurement; a delegate is 64).
(deftest-compiled-only fasl-closure-delegate.same-cost-as-in-process
  (progn
    (eval (read-from-string
           "(progn (defun %fcd-mem1 (k) (lambda (x) (+ x k)))
                   (defun %fcd-memrest (k) (lambda (&rest xs) (cons k xs))))"))
    (list (<= (abs (- (%fcd-per-call (symbol-function (intern "%FCD-MK1")) 1)
                      (%fcd-per-call (symbol-function (intern "%FCD-MEM1")) 1)))
              8)
          (<= (abs (- (%fcd-per-call (symbol-function (intern "%FCD-MKREST")) 1)
                      (%fcd-per-call (symbol-function (intern "%FCD-MEMREST")) 1)))
              8)))
  (t t))
