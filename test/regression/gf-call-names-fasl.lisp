;;; DEFGENERIC and DEFMETHOD compiled by COMPILE-FILE mark their names again when
;;; the fasl is loaded, so a one-argument call compiled later in an image that only
;;; loaded the fasl goes through the call-site cache, and runs the methods the fasl
;;; defined.

(defvar *gcnf-source* "
(defclass gcnf-k () ((a :initform 3 :accessor gcnf-a)))
(defclass gcnf-k2 (gcnf-k) ())
(defgeneric gcnf-gf (x))
(defmethod gcnf-gf ((x gcnf-k)) (values (gcnf-a x) :k))
(defmethod gcnf-implicit ((x gcnf-k)) :implicit)
(defmethod gcnf-eql ((x (eql 1))) :one)
(defmethod gcnf-eql ((x t)) :t)
")

(defvar *gcnf-names* '(gcnf-gf gcnf-implicit gcnf-eql))

(defun %gcnf-load ()
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "gf-call-names.lisp" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string *gcnf-source* s))
    (let ((fasl (compile-file src :output-file (merge-pathnames "gf-call-names.fasl" dir))))
      ;; As in an image that never compiled the DEFGENERIC / DEFMETHOD forms.
      (dolist (n *gcnf-names*) (remhash n dotcl-internal::*gf-call-names*))
      (load fasl))
    (setf dotcl:*save-sil* t)
    (eval '(defun %gcnf-use (o)
             (list (multiple-value-list (gcnf-gf o)) (gcnf-implicit o)
                   (gcnf-eql 1) (gcnf-eql 2))))))

(when (emitting-mode-p) (%gcnf-load))

(deftest-emitting-only gf-call-names-fasl.marks
  (mapcar (lambda (n) (and (gethash n dotcl-internal::*gf-call-names*) t)) *gcnf-names*)
  (t t t))

;; Under :INTERPRET the EVALed DEFUN is not compiled, so there is no SIL to read.
(deftest-compiled-only gf-call-names-fasl.gf-ic1
  (and (search "GF-IC1" (%flat-text (dotcl:function-sil (fdefinition '%gcnf-use)))) t)
  t)

(deftest-emitting-only gf-call-names-fasl.values
  (let ((o (make-instance 'gcnf-k)))
    (dotimes (i 50) (%gcnf-use o))
    (%gcnf-use o))
  ((3 :k) :implicit :one :t))

(deftest-emitting-only gf-call-names-fasl.redefine
  (let ((o (make-instance 'gcnf-k)) (o2 (make-instance 'gcnf-k2)) (out '()))
    (dotimes (i 50) (%gcnf-use o))
    (eval '(defmethod gcnf-gf ((x gcnf-k)) (values (gcnf-a x) :k-new)))
    (push (%gcnf-use o) out)
    (eval '(defmethod gcnf-gf ((x gcnf-k2)) (values 2 :k2)))
    (push (%gcnf-use o2) out)
    (push (%gcnf-use o) out)
    (remove-method #'gcnf-gf (find-method #'gcnf-gf nil (list (find-class 'gcnf-k2))))
    (push (%gcnf-use o2) out)
    (change-class o 'gcnf-k2)
    (push (car (%gcnf-use o)) out)
    (nreverse out))
  (((3 :k-new) :implicit :one :t)
   ((2 :k2) :implicit :one :t)
   ((3 :k-new) :implicit :one :t)
   ((3 :k-new) :implicit :one :t)
   (3 :k-new)))
