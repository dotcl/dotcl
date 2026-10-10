;;; bench/gf-dispatch-poly.lisp -- time of a generic function call whose call site
;;; sees several argument classes, on a warm dispatch cache.
;;;
;;; Usage (from the project root, Release):
;;;   DOTCL_NO_JIT_PROFILE=1 runtime/bin/Release/net10.0/runtime \
;;;     --core compiler/dotcl.core bench/gf-dispatch-poly.lisp
;;;   sbcl --non-interactive --load bench/gf-dispatch-poly.lisp
;;;
;;; The shape is a type checker's: a two-argument equality on a hierarchy of
;;; structure types, with a method for each pair of the same type and a default
;;; for the rest, called on a stream of pairs that mixes the types (TWO-ARG-*),
;;; and a two-argument function that dispatches on its second argument only
;;; while the first is a list (SECOND-ARG). ONE-ARG is the one-argument case.
;;; Every row runs on a warm cache: the pairs repeat, and the run before the
;;; timed ones fills it. A row is nanoseconds per call including the loop; the
;;; LOOP row is the loop calling an ordinary function, for scale.
;;;
;;; Each row is the best of five runs.

(defstruct (bty (:constructor nil)) (alias nil))
(defstruct (bvar (:include bty)) (id 0))
(defstruct (bcon (:include bty)) (name nil))
(defstruct (bapp (:include bty)) (from nil) (to nil))
(defstruct (bfun (:include bty)) (args nil))
(defstruct (bgen (:include bty)) (id 0))

(defgeneric bty= (a b))
(defmethod bty= ((a bvar) (b bvar)) (eql (bvar-id a) (bvar-id b)))
(defmethod bty= ((a bcon) (b bcon)) (eq (bcon-name a) (bcon-name b)))
(defmethod bty= ((a bapp) (b bapp)) (and (eq (bapp-from a) (bapp-from b)) (eq (bapp-to a) (bapp-to b))))
(defmethod bty= ((a bfun) (b bfun)) (eq (bfun-args a) (bfun-args b)))
(defmethod bty= ((a bgen) (b bgen)) (eql (bgen-id a) (bgen-id b)))
(defmethod bty= (a b) (declare (ignore a b)) nil)

(defgeneric bsubst (s x))
(defmethod bsubst (s (x bvar)) (if s x nil))
(defmethod bsubst (s (x bcon)) (if s x nil))
(defmethod bsubst (s (x bapp)) (if s x nil))
(defmethod bsubst (s (x bfun)) (if s x nil))
(defmethod bsubst (s x) (declare (ignore s)) x)

(defgeneric bkind (x))
(defmethod bkind ((x bvar)) 1)
(defmethod bkind ((x bcon)) 2)
(defmethod bkind ((x bapp)) 3)
(defmethod bkind ((x bty)) 0)

(defun bplain (a b) (eq a b))

(defvar *types*
  (list (make-bvar :id 1) (make-bcon :name 'int) (make-bapp :from 1 :to 2)
        (make-bfun :args nil) (make-bgen :id 3)))

;; Same-type pairs and mixed pairs: 3 classes per argument position, 9 pairs.
(defvar *pairs-3*
  (let ((ts (subseq *types* 0 3)))
    (loop for a in ts nconc (loop for b in ts collect (cons a b)))))
;; All five: 25 pairs.
(defvar *pairs-5*
  (loop for a in *types* nconc (loop for b in *types* collect (cons a b))))
(defvar *subst-pairs*
  (loop for s in (list nil (list 1)) nconc (loop for x in *types* collect (cons s x))))

(defun now () (/ (get-internal-real-time) (float internal-time-units-per-second 1d0)))

(defmacro def-pair-loop (name fn)
  `(defun ,name (pairs reps)
     (declare (fixnum reps))
     (let ((c 0))
       (declare (fixnum c))
       (dotimes (r reps)
         (dolist (p pairs)
           (when (,fn (car p) (cdr p)) (incf c))))
       c)))
(def-pair-loop run-plain bplain)
(def-pair-loop run-ty= bty=)
(def-pair-loop run-subst bsubst)

(defun run-kind (xs reps)
  (declare (fixnum reps))
  (let ((c 0))
    (declare (fixnum c))
    (dotimes (r reps)
      (dolist (x xs) (incf c (bkind x))))
    c))

(defun time-row (label fn arg calls-per-rep)
  (let* ((target 2000000)
         (reps (max 1 (floor target calls-per-rep)))
         (best most-positive-double-float))
    (funcall fn arg 1000)
    (dotimes (k 5)
      (let ((t0 (now)))
        (funcall fn arg reps)
        (setf best (min best (- (now) t0)))))
    (format t "~&~24a ~8,2f ns/call~%" label (/ (* best 1d9) (* reps calls-per-rep)))
    (finish-output)))

(format t "~&~a ~a~%" (lisp-implementation-type) (lisp-implementation-version))
(time-row "loop (plain function)" #'run-plain *pairs-3* (length *pairs-3*))
(time-row "two-arg, 3 classes" #'run-ty= *pairs-3* (length *pairs-3*))
(time-row "two-arg, 5 classes" #'run-ty= *pairs-5* (length *pairs-5*))
(time-row "second-arg, 5 classes" #'run-subst *subst-pairs* (length *subst-pairs*))
(time-row "one-arg, 5 classes" #'run-kind *types* (length *types*))
