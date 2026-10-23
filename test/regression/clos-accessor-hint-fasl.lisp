;;; A DEFCLASS compiled by COMPILE-FILE registers its readers and writers again
;;; when the fasl is loaded, so a call compiled later in an image that only
;;; loaded the fasl reads and writes the slot through the call-site cache.

(defvar *cahf-source* "
(defclass cahf-k () ((a :accessor cahf-a :initform 3) (b :reader cahf-b :initform 4)
                     (c :writer cahf-set-c :initform 5)))
")

(defun %cahf-load ()
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "clos-accessor-hint.lisp" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string *cahf-source* s))
    (let ((fasl (compile-file src :output-file (merge-pathnames "clos-accessor-hint.fasl" dir))))
      ;; As in an image that never compiled the DEFCLASS.
      (dolist (n '(cahf-a cahf-b))
        (remhash n dotcl-internal::*clos-accessor-readers*))
      (dolist (n '(cahf-a cahf-set-c))
        (remhash n dotcl-internal::*clos-accessor-writers*))
      (load fasl))
    (setf dotcl:*save-sil* t)
    (eval '(defun %cahf-use (o)
             (list (cahf-a o) (cahf-b o) (setf (cahf-a o) 7) (cahf-set-c 8 o)
                   (cahf-a o) (slot-value o 'c))))))

(when (emitting-mode-p) (%cahf-load))

(deftest-emitting-only clos-accessor-hint-fasl.values
  (funcall '%cahf-use (make-instance 'cahf-k))
  (3 4 7 8 7 8))

;; Under :INTERPRET the EVALed DEFUN is not compiled, so there is no SIL to read.
(deftest-compiled-only clos-accessor-hint-fasl.reader-ic
  (and (search "READER-IC" (%flat-text (dotcl:function-sil (fdefinition '%cahf-use)))) t)
  t)

(deftest-compiled-only clos-accessor-hint-fasl.writer-ic
  (and (search "WRITER-IC" (%flat-text (dotcl:function-sil (fdefinition '%cahf-use)))) t)
  t)
