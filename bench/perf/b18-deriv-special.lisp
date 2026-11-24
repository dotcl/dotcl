(load "bench/perf/pcommon.lisp")
(setf *bench-group* "18-deriv-special")
;; Symbolic differentiation (MAPCAR with a lambda, LIST, CONS) and a dynamic binding of a special variable
;; read through a function: two small loops from a benchmark suite where dotcl is 5-8 times SBCL.
(defvar *sink* nil)
(defun deriv (a)
  (cond ((not (consp a)) (if (eq a 'x) 1 0))
        ((eq (car a) '+) (cons '+ (mapcar #'deriv (cdr a))))
        ((eq (car a) '-) (cons '- (mapcar #'deriv (cdr a))))
        ((eq (car a) '*) (list '* a (cons '+ (mapcar (lambda (b) (list '/ (deriv b) b)) (cdr a)))))
        ((eq (car a) '/) (list '- (list '/ (deriv (cadr a)) (caddr a))
                               (list '/ (cadr a) (list '* (caddr a) (caddr a) (deriv (caddr a))))))
        (t 'error)))
(defvar *sp* 0)
(defun use-sp () *sp*)
(bench "deriv of (+ (* 3 x x) (* a x x) (* b x) 5) x1000" 200
       (let ((r nil)) (dotimes (i 1000) (setq r (deriv '(+ (* 3 x x) (* a x x) (* b x) 5)))) (setq *sink* r)))
(bench "special binding + read through a function x1000" 2000
       (let ((s 0)) (dotimes (i 1000) (let ((*sp* i)) (setq s (logand (+ s (use-sp)) #xFFFFFF)))) (setq *sink* s)))
