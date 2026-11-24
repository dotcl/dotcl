;;; regression: skip-under-interp
;;; A LOAD-TIME-VALUE form in a function whose body is compiled for several
;;; entries (&key, &optional, &rest, typed arguments), or in a DEFSTRUCT slot
;;; initform, is evaluated once, not once per entry.

(defvar *lvo-count* 0)
(defun lvo-tick () (incf *lvo-count*) 7)

(defun lvo-count-of (thunk)
  (setf *lvo-count* 0)
  (funcall thunk)
  *lvo-count*)

(deftest-compiled-only ltv-evaluated-once.key-default
  (lvo-count-of (lambda () (eval '(defun lvo-k1 (&key (x (load-time-value (lvo-tick)))) x))))
  1)
(deftest-compiled-only ltv-evaluated-once.key-body
  (lvo-count-of (lambda () (eval '(defun lvo-k2 (&key a) (list a (load-time-value (lvo-tick)))))))
  1)
(deftest-compiled-only ltv-evaluated-once.optional
  (lvo-count-of (lambda () (eval '(defun lvo-o1 (&optional (a (load-time-value (lvo-tick))) b) (list a b)))))
  1)
(deftest-compiled-only ltv-evaluated-once.rest
  (lvo-count-of (lambda () (eval '(defun lvo-r1 (&rest r) (list r (load-time-value (lvo-tick)))))))
  1)
(deftest-compiled-only ltv-evaluated-once.defstruct
  (lvo-count-of (lambda () (eval '(defstruct lvo-s (x (load-time-value (lvo-tick))) (y 2)))))
  1)

;; Two forms that look alike are two evaluations; the value is shared by every entry.
(deftest-compiled-only ltv-evaluated-once.two-sites
  (lvo-count-of (lambda () (eval '(defun lvo-t1 (&key (a (load-time-value (lvo-tick)))
                                                      (b (load-time-value (lvo-tick))))
                                    (list a b)))))
  2)
(deftest-compiled-only ltv-evaluated-once.values
  (progn (eval '(defun lvo-v1 (&key (x (load-time-value (list 1 2)))) x))
         (list (lvo-v1) (eq (lvo-v1) (lvo-v1)) (lvo-v1 :x 5)))
  ((1 2) t 5))
(deftest-compiled-only ltv-evaluated-once.struct-value
  (progn (eval '(defstruct lvo-s2 (x (load-time-value (list 3)))))
         (let ((a (make-lvo-s2)) (b (make-lvo-s2)))
           (list (lvo-s2-x a) (eq (lvo-s2-x a) (lvo-s2-x b)) (lvo-s2-x (make-lvo-s2 :x 9)))))
  ((3) t 9))

;; compile-file: the form runs once per load of the fasl.
(defun lvo-fasl (name forms)
  (let* ((dir (regression-temp-dir/))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(deftest-compiled-only ltv-evaluated-once.fasl-load
  (let ((fasl (lvo-fasl "lvo-f1"
                        '((defun lvo-f-k (&key (x (load-time-value (lvo-tick)))) x)
                          (defun lvo-f-o (&optional a) (list a (load-time-value (lvo-tick))))
                          (defstruct lvo-f-s (x (load-time-value (lvo-tick))))))))
    (list (lvo-count-of (lambda () (load fasl)))
          (lvo-count-of (lambda () (load fasl)))))
  (3 3))
