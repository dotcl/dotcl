;;; A function that binds nothing reads and SETQs the special variables that an
;;; earlier compiled function binds through its thread's binding stack, fetched
;;; at the first such access and kept for the rest of the call.

(defvar *slz-x*)
(defvar *slz-v*)
(defvar *slz-u*)
(defvar *slz-g* :global)
(defvar *slz-late* :late-global)

;; The binders come first, so the readers below are compiled knowing that
;; these variables get bound.
(defun %slz-outer (*slz-x* y)
  (let ((*slz-u* nil) (*slz-v* nil) (r nil))
    (loop while *slz-x*
          do (push (%slz-step y) r)
             (setq *slz-x* (cdr *slz-x*)))
    (list (nreverse r) *slz-u*)))

(defun %slz-bind-g (v f)
  (let ((*slz-g* v)) (funcall f)))

(defun %slz-bind-late (f)
  (let ((*slz-late* :bound)) (funcall f)))

;; The PTIMES1 / PTIMES3 shape of frpoly: the caller binds, a helper that binds
;; nothing reads and SETQs the bindings in a loop.
(defun %slz-step (y)
  (loop while y
        do (setq *slz-u* (cons (+ (car *slz-x*) (car y)) *slz-u*))
           (setq *slz-v* (car *slz-u*))
           (setq y (cdr y)))
  (list *slz-v* (length *slz-u*)))

(defun %slz-read-g () *slz-g*)
(defun %slz-setq-g (v) (setq *slz-g* v) *slz-g*)

;; Reads a variable that has not been bound yet at run time, then calls a
;; function that binds it for the first time, then reads it again in the same
;; call.
(defun %slz-late (f)
  (let ((a *slz-late*)
        (b (funcall f))
        (c *slz-late*))
    (list a b c)))

(defun %slz-values () (values *slz-g* *slz-x*))

(defun %slz-read-u () (handler-case *slz-u* (unbound-variable () :unbound)))

(deftest special-lazy-stack.frpoly-shape
  (list (%slz-outer '(10 20) '(1 2 3)) (boundp '*slz-x*) (boundp '*slz-u*))
  ((((13 3) (23 6)) (23 22 21 13 12 11)) nil nil))

(deftest special-lazy-stack.global-and-bound
  (list (%slz-read-g)
        (%slz-bind-g :inner #'%slz-read-g)
        (%slz-bind-g :inner (lambda () (%slz-setq-g :set-inside)))
        (%slz-read-g)
        (%slz-setq-g :set-outside)
        (%slz-bind-g :inner #'%slz-read-g)
        (prog1 (%slz-read-g) (setq *slz-g* :global)))
  (:global :inner :set-inside :global :set-outside :inner :set-outside))

(deftest special-lazy-stack.first-binding-mid-call
  (%slz-late (lambda () (%slz-bind-late (lambda () (%slz-late (lambda () :leaf))))))
  (:late-global (:bound :leaf :bound) :late-global))

(deftest special-lazy-stack.values
  (%slz-bind-g 7 (lambda () (let ((*slz-x* 8)) (multiple-value-list (%slz-values)))))
  (7 8))

(deftest special-lazy-stack.unbound
  (list (%slz-read-u)
        (let ((*slz-u* 1)) (list (%slz-read-u)
                                 (progv '(*slz-u*) '() (%slz-read-u))
                                 (progn (makunbound '*slz-u*) (%slz-read-u))))
        (%slz-read-u))
  (:unbound (1 :unbound :unbound) :unbound))

;; A binding on one thread is not seen by a reader on another.
(deftest special-lazy-stack.threads
  (let* ((threads (loop for k below 4
                        collect (let ((k k))
                                  (dotcl:make-thread
                                   (lambda ()
                                     (remove-duplicates
                                      (loop repeat 200
                                            collect (if (evenp k)
                                                        (%slz-bind-g k #'%slz-read-g)
                                                        (%slz-read-g)))))))))
         (results (mapcar #'dotcl:thread-join threads)))
    (list results (%slz-read-g)))
  (((0) (:global) (2) (:global)) :global))

(defvar *slz-never* :never)
(defvar *slz-saved-save-sil* dotcl:*save-sil*)
(setf dotcl:*save-sil* t)
(defun %slz-shape () (list *slz-g* *slz-x*))
(defun %slz-shape-global () (setq *slz-never* (list *slz-never*)))
(setf dotcl:*save-sil* *slz-saved-save-sil*)

;; Variables nothing has bound keep the global value's inline path.
(deftest-compiled-only special-lazy-stack.shape
  (let ((s (%flat-text (dotcl:function-sil #'%slz-shape)))
        (g (%flat-text (dotcl:function-sil #'%slz-shape-global))))
    (list (and (search "DynamicBindings.GetLazy" s) t)
          (search "DynamicBindings.Here" s)
          (search "DynamicBindings.Get)" s)
          (search "Lazy" g)
          (and (search "DynamicBindings.Get)" g) (search "DynamicBindings.SetChecked)" g) t)
          (%slz-shape-global)))
  (t nil nil nil t (:never)))
