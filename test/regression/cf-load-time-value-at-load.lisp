;;; In a compiled file, a LOAD-TIME-VALUE form is evaluated when the file is
;;; loaded (CLHS 3.2.2.2), each time it is loaded. It was evaluated the first
;;; time the code containing it ran, and only once per process for a given
;;; fasl: loading the fasl again kept the old value and ran nothing. Coalton
;;; registers the instance choices of compiled code with a LOAD-TIME-VALUE and
;;; expects a stale choice to signal an error while the fasl is loaded.

(defvar *cfltv-count* 0)
(defvar *cfltv-order* '())

(defun %cfltv-compile (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))))

(deftest-emitting-only cf-load-time-value-at-load.order-and-reload
  (let ((fasl (%cfltv-compile
               "cfltv"
               '("(push :before *cfltv-order*)"
                 "(defun cfltv-f () (load-time-value (progn (push :ltv *cfltv-order*) (incf *cfltv-count* 100))))"
                 "(push :after *cfltv-order*)"))))
    (setf *cfltv-count* 0 *cfltv-order* '())
    (load fasl)
    (let ((first (list (reverse *cfltv-order*) *cfltv-count*)))
      (load fasl)
      (list first (funcall 'cfltv-f) *cfltv-count*)))
  (((:before :ltv :after) 100) 200 200))

;;; A macro that calls EVAL while COMPILE-FILE compiles a top level form gets
;;; the value of the LOAD-TIME-VALUE in what it evaluates, now. The evaluation
;;; had taken the LOAD-TIME-VALUE for the fasl being written and answered NIL
;;; (Coalton looks up its constants this way: (coalton (and)) compiled to NIL).
;;; Compiled-only: under :INTERPRET the EVAL interprets, which is not what this
;;; is about.
(deftest-compiled-only cf-load-time-value-at-load.eval-in-macro
  (let ((fasl (%cfltv-compile
               "cfltv2"
               '("(defmacro cfltv-m () `',(eval '(car (load-time-value (list 42)))))"
                 "(defun cfltv-g () (cfltv-m))"))))
    (load fasl)
    (funcall 'cfltv-g))
  42)

;;; Each load has its own value: a function from an earlier load keeps the
;;; value of that load, and a NIL value is not evaluated again.
(deftest-emitting-only cf-load-time-value-at-load.per-load-value
  (let ((fasl (%cfltv-compile
               "cfltv3"
               '("(defun cfltv-h () (load-time-value (incf *cfltv-count*)))"
                 "(defun cfltv-n () (load-time-value (progn (incf *cfltv-count* 10) nil)))"))))
    (setf *cfltv-count* 0)
    (load fasl)
    (let ((old (fdefinition 'cfltv-h))
          (a (list (funcall 'cfltv-h) (funcall 'cfltv-h) (funcall 'cfltv-n) (funcall 'cfltv-n))))
      (load fasl)
      (list a (funcall 'cfltv-h) (funcall old) (funcall 'cfltv-n) *cfltv-count*)))
  ((1 1 nil nil) 12 1 nil 22))
