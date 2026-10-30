;;; An untyped DEFUN whose body only checks its arguments, compares, reads
;;; constants and takes CAR is entered without linking a frame. A failing
;;; argument check, or CAR of something else than a list, still shows the
;;; function with its arguments on top of the backtrace, and calls that hand
;;; such a function a frame leave the frame list as it was.

(declaim (notinline %dlm-ord %dlm-first))
(defun %dlm-ord (a b)
  (declare (type (member :lt :eq :gt) a b))
  (if (eq a b) t nil))
(defun %dlm-first (x) (car x))
(defun %dlm-tail (a b) (%dlm-ord a b))
(defun %dlm-t0 (f g a b) (funcall f g a b))

(defvar *dl-loaded*
  (when (compiled-mode-p)
    (let ((fasl (regression-temp-file "direct-leaf.fasl")))
      (compile-file "test/regression/direct-leaf/defs.lisp" :output-file fasl)
      (load fasl)
      t)))

(defun %dl-leaf-p (f n) (dotnet:static "DotCL.LispFunction" "LeafEntryP" f n))

(defun %dl-top-frame (thunk)
  (block out
    (handler-bind ((error (lambda (c)
                            (return-from out
                              (let ((s (with-output-to-string (o) (dotcl:print-backtrace o))))
                                (list (type-of c)
                                      (string-trim " " (subseq s (1+ (position #\: s))
                                                               (position #\Newline s)))))))))
      (funcall thunk))))

(defun %dl-depth () (length (dotcl:backtrace)))
(defun %dl-outer () (list (%dl-depth)))

(deftest direct-leaf.values
  (list (%dlm-ord :lt :lt) (%dlm-ord :lt :gt) (funcall #'%dlm-ord :eq :eq)
        (%dlm-first '(1 2)) (%dlm-first nil) (%dlm-tail :gt :gt)
        (%dlm-t0 (lambda (g a b) (funcall g a b)) #'%dlm-tail :eq :eq))
  (t nil t 1 nil t t))

(deftest-emitting-only direct-leaf.entries
  (list (%dl-leaf-p #'%dlm-ord 2) (%dl-leaf-p #'%dlm-first 1) (%dl-leaf-p #'%dlm-tail 2))
  (t t nil))

(deftest-emitting-only direct-leaf.backtrace
  (let ((base (%dl-depth)))
    (list (%dl-top-frame (lambda () (%dlm-ord :lt 3)))
          (%dl-top-frame (lambda () (funcall #'%dlm-ord :foo :lt)))
          (%dl-top-frame (lambda () (%dlm-first 5)))
          (%dlm-t0 (lambda (g a b) (funcall g a b)) #'%dlm-tail :eq :eq)
          (- (car (%dl-outer)) base)
          (%dlm-t0 (lambda (g a b) (declare (ignore g)) (%dlm-ord a b)) nil :lt :lt)
          (- (car (%dl-outer)) base)
          (- (%dl-depth) base)))
  ((type-error "(%DLM-ORD :LT 3)")
   (type-error "(%DLM-ORD :FOO :LT)")
   (type-error "(%DLM-FIRST 5)")
   t 1 t 1 0))

(deftest-compiled-only direct-leaf.fasl
  (let ((base (%dl-depth)))
    (list (list (%dl-leaf-p #'cl-user::%dlf-ord 2) (%dl-leaf-p #'cl-user::%dlf-first 1)
                (%dl-leaf-p #'cl-user::%dlf-ltv 1) (%dl-leaf-p #'cl-user::%dlf-ltv-car 1)
                (%dl-leaf-p #'cl-user::%dlf-tail 2))
          (cl-user::%dlf-ord :lt :lt) (cl-user::%dlf-ord :lt :eq) (cl-user::%dlf-first '(a))
          (cl-user::%dlf-ltv :x) (cl-user::%dlf-ltv :y) (cl-user::%dlf-ltv-car :x)
          (eq (cl-user::%dlf-ltv :x) (cl-user::%dlf-ltv :x))
          (%dl-top-frame (lambda () (cl-user::%dlf-ord :lt 3)))
          (%dl-top-frame (lambda () (cl-user::%dlf-first 5)))
          (%dl-top-frame (lambda () (cl-user::%dlf-ltv :z)))
          (cl-user::%dlf-t0 (lambda (g a b) (funcall g a b)) #'cl-user::%dlf-tail :eq :eq)
          (- (car (%dl-outer)) base)
          (cl-user::%dlf-t0 (lambda (g a b) (funcall g a b)) #'cl-user::%dlf-let-tail :eq :eq)
          (- (car (%dl-outer)) base)
          (cl-user::%dlf-let-tail :gt :lt)
          (multiple-value-list (cl-user::%dlf-ltv :x))
          (multiple-value-list (progn (floor 7 2) (cl-user::%dlf-ltv-let)))
          (%dl-leaf-p #'cl-user::%dlf-ltv-let 0)
          (- (%dl-depth) base)))
  ((t t t t nil) t nil cl-user::a (1 2) :none 1 t
   (type-error "(%DLF-ORD :LT 3)")
   (type-error "(%DLF-FIRST 5)")
   (type-error "(%DLF-LTV :Z)")
   t 1 t 1 nil ((1 2)) (3) t 0))
