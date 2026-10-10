;;; The accessor of a :READ-ONLY slot of a standard DEFSTRUCT is inlined at
;;; call sites like any other accessor. It used to be left out of the registry
;;; of inlinable accessors together with its SETF expansion, so every read of
;;; a read-only slot was a full function call (and, for a raw fixnum slot, an
;;; out-of-line read). Type checkers built on read-only structures read such
;;; slots in their innermost loops. Only the SETF expansion is left out now:
;;; writing a read-only slot is still an error.
;;;
;;; Both registrations are covered: the one DEFSTRUCT's expansion makes, and
;;; the one a fasl makes when it is loaded into an image that never expanded
;;; the DEFSTRUCT itself.

(setf dotcl:*save-sil* t)

(defun %sroa-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %sroa-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defstruct sroa-base (tag nil))
(defstruct (sroa (:include sroa-base))
  (id 0 :type fixnum :read-only t)
  (kind nil :read-only t)
  (note nil))

(defun %sroa-id (x) (sroa-id x))
(defun %sroa-kind (x) (sroa-kind x))
(defun %sroa-note (x) (sroa-note x))
(defun %sroa-set-kind (x v) (funcall (compile nil `(lambda (y) (setf (sroa-kind y) ',v))) x))

(deftest struct-read-only-accessor-inline.values
  (let ((s (make-sroa :tag :t :id 123456789 :kind :star :note "n")))
    (list (%sroa-id s) (%sroa-kind s) (%sroa-note s) (sroa-base-tag s)))
  (123456789 :star "n" :t))

(deftest struct-read-only-accessor-inline.many-values
  (let ((s (make-sroa :id 7 :kind :k)))
    (multiple-value-list (%sroa-kind s)))
  (:k))

(deftest struct-read-only-accessor-inline.setf-still-an-error
  (let ((s (make-sroa :id 1 :kind :a)))
    (list (handler-case (progn (%sroa-set-kind s :b) :no-error)
            (error () :error))
          (sroa-kind s)))
  (:error :a))

(deftest-emitting-only struct-read-only-accessor-inline.read-only-inlined
  (list (%sroa-count "Runtime.StructRefI" (%sroa-sil #'%sroa-id))
        (%sroa-count "Runtime.StructRefI" (%sroa-sil #'%sroa-kind))
        (%sroa-count "Runtime.StructRefI" (%sroa-sil #'%sroa-note)))
  (1 1 1))

;;; ---- the registration a fasl makes when it is loaded ----

(defun %sroa-compile-file (name text)
  (let* ((dir (substitute #\/ #\\ (format nil "~a/dotcl-sroa-~a/" (regression-temp-dir)
                                          (get-internal-real-time))))
         (src (concatenate 'string dir name ".lisp"))
         (fasl (concatenate 'string dir name ".fasl")))
    (ensure-directories-exist dir)
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(in-package ~s)~%" (package-name *package*))
      (write-string text s))
    (compile-file src :output-file fasl)
    fasl))

;; Compiled mode only: the DEFUNs are made by EVAL, which the :INTERPRET mode
;; does not compile.
(deftest-compiled-only struct-read-only-accessor-inline.from-fasl
  (let ((fasl (%sroa-compile-file
               "sroa-f"
               "(defstruct sroa-f (a 0 :read-only t) (b 0))")))
    ;; Forget what compiling the file recorded, so only LOAD can put it back.
    (remhash 'sroa-f-a dotcl-internal::*struct-accessors*)
    (remhash 'sroa-f-b dotcl-internal::*struct-accessors*)
    (load fasl)
    (eval '(defun %sroa-f-a (x) (sroa-f-a x)))
    (eval '(defun %sroa-f-b (x) (sroa-f-b x)))
    (list (%sroa-count "Runtime.StructRefI" (%sroa-sil #'%sroa-f-a))
          (%sroa-count "Runtime.StructRefI" (%sroa-sil #'%sroa-f-b))
          (funcall '%sroa-f-a (funcall 'make-sroa-f :a 3 :b 4))))
  (1 1 3))
