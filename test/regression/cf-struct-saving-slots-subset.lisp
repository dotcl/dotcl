;;; A structure literal in a fasl whose MAKE-LOAD-FORM is
;;; MAKE-LOAD-FORM-SAVING-SLOTS with :SLOT-NAMES naming only some slots (or any
;;; creation form of that shape that does not set every slot) is created when
;;; the fasl is loaded and found by its key afterwards. Making it called
;;; ALLOCATE-INSTANCE and kept the result only when it was a standard instance,
;;; so a structure was made and dropped, and the literal's own reference failed
;;; with "load-form literal ... was not created before it was referenced".
;;; Coalton's type structures go this way, and Coalton could not be compiled.

(defun %cfsss-compile-load (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (let ((*error-output* (make-broadcast-stream)))
      (load (compile-file src :output-file fasl)))
    t))

(deftest-emitting-only cf-struct-saving-slots-subset.load
  (progn
    (%cfsss-compile-load
     "cfsss"
     '("(eval-when (:compile-toplevel :load-toplevel :execute)"
       "  (defstruct cfsss-pt x y)"
       "  (defmethod make-load-form ((p cfsss-pt) &optional env)"
       "    (make-load-form-saving-slots p :slot-names '(x) :environment env)))"
       "(defmacro cfsss-lit (form) `',(eval form))"
       "(defun cfsss-p1 () (cfsss-lit (make-cfsss-pt :x 1 :y 2)))"
       "(defparameter *cfsss-both* (cfsss-lit (let ((p (make-cfsss-pt :x 3))) (list p p))))"))
    (list (funcall 'cfsss-pt-x (funcall 'cfsss-p1))
          (funcall 'cfsss-pt-p (funcall 'cfsss-p1))
          (let ((b (symbol-value '*cfsss-both*))) (eq (first b) (second b)))))
  (1 t t))
