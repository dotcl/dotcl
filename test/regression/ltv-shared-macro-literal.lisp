;;; A macro that returns one literal (LOAD-TIME-VALUE ...) list for every
;;; expansion gets one evaluation per occurrence, not one for all of them.

(defmacro lsm-site () '(load-time-value (cons nil nil)))
(defmacro lsm-site-in-let () `(let ((p (load-time-value (cons nil nil)))) p))

(deftest-compiled-only ltv-shared-macro-literal.eval
  (eval '(let ((a (lsm-site)) (b (lsm-site))) (eq a b)))
  nil)
(deftest-compiled-only ltv-shared-macro-literal.defun
  (progn (eval '(defun lsm-f () (let ((a (lsm-site)) (b (lsm-site))) (eq a b))))
         (lsm-f))
  nil)
(deftest-compiled-only ltv-shared-macro-literal.backquote
  (progn (eval '(defun lsm-g () (list (lsm-site-in-let) (lsm-site-in-let))))
         (let ((r (lsm-g))) (eq (first r) (second r))))
  nil)
;; One occurrence is still one evaluation however often it runs.
(deftest-compiled-only ltv-shared-macro-literal.same-site
  (progn (eval '(defun lsm-h () (lsm-site)))
         (eq (lsm-h) (lsm-h)))
  t)
;; The entries of one function body share the evaluation of an expanded site.
(deftest-compiled-only ltv-shared-macro-literal.entries
  (progn (eval '(defun lsm-k (&key (x (lsm-site))) x))
         (eq (lsm-k) (lsm-k)))
  t)

;; The evaluation count of an expanded site is the same as for a written one.
(defvar *lsm-count* 0)
(defun lsm-tick () (incf *lsm-count*) (list *lsm-count*))
(defun lsm-count-of (thunk)
  (setf *lsm-count* 0)
  (funcall thunk)
  *lsm-count*)
(defmacro lsm-tick-site () '(load-time-value (lsm-tick)))

;; Several entries of one body (&key, &optional, &rest, typed arguments).
(deftest-compiled-only ltv-shared-macro-literal.entries-count
  (list (lsm-count-of (lambda () (eval '(defun lsm-e1 (&key (x (lsm-tick-site))) x))))
        (lsm-count-of (lambda () (eval '(defun lsm-e2 (&optional (a (lsm-tick-site)) b) (list a b)))))
        (lsm-count-of (lambda () (eval '(defun lsm-e3 (&rest r) (list r (lsm-tick-site))))))
        (lsm-count-of (lambda () (eval '(defun lsm-e4 (&key (n 1) a) (declare (fixnum n)) (list a n (lsm-tick-site)))))))
  (1 1 1 1))
(deftest-compiled-only ltv-shared-macro-literal.two-occurrences-count
  (lsm-count-of (lambda () (eval '(defun lsm-e5 (&key a) (list a (lsm-tick-site) (lsm-tick-site))))))
  2)

;; MACROLET, a macro that expands to another macro, a macro used inside a macro.
(deftest-compiled-only ltv-shared-macro-literal.macrolet
  (eval '(macrolet ((s () '(load-time-value (cons nil nil))))
          (let ((a (s)) (b (s))) (list (eq a b) (eq a (s))))))
  (nil nil))
(deftest-compiled-only ltv-shared-macro-literal.macrolet-entries
  (progn (eval '(macrolet ((s () '(load-time-value (cons nil nil))))
                 (defun lsm-m1 (&key (x (s))) x)))
         (eq (lsm-m1) (lsm-m1)))
  t)
(defmacro lsm-inner () '(load-time-value (cons nil nil)))
(defmacro lsm-outer () (list 'lsm-inner))
(defmacro lsm-outer2 () (list 'list (list 'lsm-outer) (list 'lsm-inner)))
(defmacro lsm-tinner () '(load-time-value (lsm-tick)))
(defmacro lsm-touter () (list 'list (list 'lsm-tinner) (list 'lsm-tinner)))
(deftest-compiled-only ltv-shared-macro-literal.nested
  (list (eval '(let ((a (lsm-outer)) (b (lsm-outer))) (eq a b)))
        (let ((r (eval '(lsm-outer2)))) (eq (first r) (second r)))
        (lsm-count-of (lambda () (eval '(defun lsm-n1 (&key a) (list a (lsm-touter)))))))
  (nil nil 2))

;; COMPILE of one source form twice is two compiles: two evaluations.
(deftest-compiled-only ltv-shared-macro-literal.recompile
  (let ((form '(lambda () (lsm-tick-site))))
    (list (lsm-count-of (lambda () (compile nil form)))
          (lsm-count-of (lambda () (compile nil form)))
          (lsm-count-of (lambda () (compile nil form) (compile nil form)))))
  (1 1 2))

;; COMPILE-FILE: one evaluation per occurrence per load of the fasl.
(defun lsm-fasl (name forms)
  (let* ((dir (regression-temp-dir/))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))
(deftest-compiled-only ltv-shared-macro-literal.fasl
  (let ((fasl (lsm-fasl "lsm-f1"
                        '((defun lsm-fa () (let ((a (lsm-tick-site)) (b (lsm-tick-site))) (eq a b)))
                          (defun lsm-fb (&key (x (lsm-tick-site))) x)))))
    (list (lsm-count-of (lambda () (load fasl)))
          (progn (funcall 'lsm-fa))
          (eq (funcall 'lsm-fb) (funcall 'lsm-fb))
          (lsm-count-of (lambda () (load fasl)))))
  (3 nil t 3))
