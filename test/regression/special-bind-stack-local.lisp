;;; A function that binds a special variable takes its thread's binding stack
;;; once and hands it to every bind, read, SETQ and unbind in its body.

(defvar *sbsl-x*)
(defvar *sbsl-y*)
(defvar *sbsl-z*)
(defvar *sbsl-g* :global)
(defvar *sbsl-u*)

;; stak from the Gabriel benchmarks: reads in the init forms of nested LETs.
(defun %sbsl-stak-aux ()
  (if (not (< (the fixnum *sbsl-y*) (the fixnum *sbsl-x*)))
      *sbsl-z*
      (let ((*sbsl-x* (let ((*sbsl-x* (the fixnum (1- (the fixnum *sbsl-x*))))
                            (*sbsl-y* *sbsl-y*)
                            (*sbsl-z* *sbsl-z*))
                        (%sbsl-stak-aux)))
            (*sbsl-y* (let ((*sbsl-x* (the fixnum (1- (the fixnum *sbsl-y*))))
                            (*sbsl-y* *sbsl-z*)
                            (*sbsl-z* *sbsl-x*))
                        (%sbsl-stak-aux)))
            (*sbsl-z* (let ((*sbsl-x* (the fixnum (1- (the fixnum *sbsl-z*))))
                            (*sbsl-y* *sbsl-x*)
                            (*sbsl-z* *sbsl-y*))
                        (%sbsl-stak-aux))))
        (%sbsl-stak-aux))))

(defun %sbsl-stak (*sbsl-x* *sbsl-y* *sbsl-z*)
  (%sbsl-stak-aux))

;; SETQ inside a binding function, of the bound variable and of a global one;
;; the binding is undone, the global assignment stays.
(defun %sbsl-setq (v)
  (let ((*sbsl-x* 1))
    (setq *sbsl-x* (+ *sbsl-x* v))
    (setq *sbsl-g* v)
    (list *sbsl-x* *sbsl-g*)))

;; Several values out of a binding function, and of a parameter that is special.
(defun %sbsl-values (*sbsl-x*)
  (let ((*sbsl-y* (* 2 *sbsl-x*)))
    (values *sbsl-x* *sbsl-y* (+ *sbsl-x* *sbsl-y*))))

;; PROGV with no value inside a binding function: the variable reads as unbound.
(defun %sbsl-progv-unbound ()
  (let ((*sbsl-u* 1))
    (list *sbsl-u*
          (progv '(*sbsl-u*) '()
            (handler-case *sbsl-u* (unbound-variable () :unbound)))
          *sbsl-u*)))

;; A non-local exit out of nested bindings.
(defun %sbsl-throw (n)
  (let ((*sbsl-x* n))
    (if (zerop n)
        (throw 'sbsl *sbsl-x*)
        (let ((*sbsl-y* n)) (%sbsl-throw (1- n))))))

(deftest special-bind-stack-local.stak
  (list (%sbsl-stak 18 12 6) (%sbsl-stak 12 8 4))
  (7 5))

(deftest special-bind-stack-local.setq
  (list (%sbsl-setq 5)
        *sbsl-g*
        (boundp '*sbsl-x*)
        (progn (setq *sbsl-g* :global) *sbsl-g*))
  ((6 5) 5 nil :global))

(deftest special-bind-stack-local.values
  (multiple-value-list (%sbsl-values 3))
  (3 6 9))

(deftest special-bind-stack-local.progv-unbound
  (%sbsl-progv-unbound)
  (1 :unbound 1))

(deftest special-bind-stack-local.throw
  (list (catch 'sbsl (%sbsl-throw 5)) (boundp '*sbsl-x*) (boundp '*sbsl-y*))
  (0 nil nil))

;; Each thread binds on its own stack.
(deftest special-bind-stack-local.threads
  (let* ((threads (loop for k below 4
                        collect (let ((k k))
                                  (dotcl:make-thread
                                   (lambda ()
                                     (loop repeat 50 collect (%sbsl-stak (+ 12 k) 8 4)
                                           into r
                                           finally (return (remove-duplicates r))))))))
         (results (mapcar #'dotcl:thread-join threads)))
    (list results (%sbsl-stak 12 8 4)))
  (((5) (8) (5) (8)) 5))

(defvar *sbsl-saved-save-sil* dotcl:*save-sil*)
(setf dotcl:*save-sil* t)
(defun %sbsl-shape (a)
  (let ((*sbsl-x* a))
    (let ((*sbsl-y* *sbsl-x*))
      (list *sbsl-x* *sbsl-y*))))
(setf dotcl:*save-sil* *sbsl-saved-save-sil*)

(deftest-emitting-only special-bind-stack-local.one-fetch
  (let ((s (%flat-text (dotcl:function-sil #'%sbsl-shape))))
    (list (%sbsl-shape 4)
          (and (search "DynamicBindings.Here" s) t)
          (search "DynamicBindings.Here" s :start2 (1+ (search "DynamicBindings.Here" s)))
          (search "DynamicBindings.Push)" s)
          (search "DynamicBindings.Get)" s)
          (search "DynamicBindings.BindDepth)" s)))
  ((4 4) t nil nil nil nil))
