;;; A global special that (declaim (fixnum VAR)) declared is read as a fixnum by the typed paths: a
;;; comparison or arithmetic on it is a long operation. The check of its value stays (a binding or a SETQ
;;; of a special is not checked), so a value that is not a fixnum still signals a type error where it is used.

(defvar *pfs-end* 100)
(declaim (fixnum *pfs-end*))
(defvar *pfs-lim* 10)
(declaim (type fixnum *pfs-lim*))
(defvar *pfs-any* 5)
(declaim (type (or null fixnum) *pfs-any*))

(defun pfs-lt (pos) (declare (fixnum pos)) (< pos *pfs-end*))
(defun pfs-arith (pos) (declare (fixnum pos)) (+ pos *pfs-lim* (* 2 *pfs-end*)))
(defun pfs-loop () (let ((n 0)) (declare (fixnum n)) (dotimes (i *pfs-lim* n) (incf n i))))
(defun pfs-any (pos) (declare (fixnum pos)) (and *pfs-any* (< pos *pfs-any*)))
(defun pfs-closure (next) (declare (function next))
  (lambda (pos) (declare (fixnum pos)) (and (< pos *pfs-end*) (funcall next (1+ pos)))))

(defun pfs-error (thunk) (handler-case (funcall thunk) (type-error () :type-error) (error () :other-error)))

(deftest proclaimed-fixnum-special.values
  (list (pfs-lt 5) (pfs-lt 100) (pfs-lt 500) (pfs-arith 1) (pfs-loop) (pfs-any 3) (pfs-any 9)
        (let ((*pfs-end* 3)) (list (pfs-lt 2) (pfs-lt 3)))
        (let ((*pfs-end* (1- most-positive-fixnum))) (pfs-lt 5))
        (funcall (pfs-closure (lambda (p) p)) 5)
        (let ((*pfs-end* 7)) (funcall (pfs-closure (lambda (p) p)) 5))
        (let ((*pfs-end* 7)) (funcall (pfs-closure (lambda (p) p)) 9)))
  (t nil nil 211 45 t nil (t nil) t 6 6 nil))

(deftest-emitting-only proclaimed-fixnum-special.value-that-is-not-a-fixnum-still-errors
  (list (pfs-error (lambda () (let ((*pfs-end* "x")) (pfs-lt 5))))
        (pfs-error (lambda () (let ((*pfs-end* (expt 10 30))) (pfs-lt 5))))
        (pfs-error (lambda () (let ((*pfs-end* 1.5d0)) (pfs-lt 5))))
        (pfs-error (lambda () (let ((*pfs-lim* nil)) (pfs-arith 1))))
        (pfs-error (lambda () (let ((*pfs-end* "x")) (funcall (pfs-closure (lambda (p) p)) 5)))))
  (:type-error :type-error :type-error :type-error :type-error))
