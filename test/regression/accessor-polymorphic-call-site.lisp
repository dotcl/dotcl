;;; An accessor called in a method on a superclass sees instances of every
;;; subclass. Its call-site cache holds one class; changing class used to
;;; allocate a new cache entry each time. The entries are now kept per class
;;; layout and reused.

(defclass gfm-sbase () ((n :initform 0 :accessor gfm-n)))
(defclass gfm-s1 (gfm-sbase) ((a :initform 1)))
(defclass gfm-s2 (gfm-sbase) ((b :initform 2) (c :initform 3)))
(defclass gfm-s3 (gfm-s2) ())
(defun %gfm-bump (x) (incf (gfm-n x)))
(defvar *gfm-sobjs* (vector (make-instance 'gfm-s1) (make-instance 'gfm-s2) (make-instance 'gfm-s3)))

(defun %gfm-bytes2 () (nth 4 (dotcl:gc-stats)))

(defun %gfm-reset () (map nil (lambda (x) (setf (gfm-n x) 0)) *gfm-sobjs*))

(deftest gf-dispatch-many-classes.accessor-rotating
  (let ((v *gfm-sobjs*))
    (%gfm-reset)
    (dotimes (k 30) (%gfm-bump (aref v (mod k 3))))
    (map 'list #'gfm-n v))
  (10 10 10))

(deftest-compiled-only gf-dispatch-many-classes.accessor-rotating-no-alloc
  (let ((v *gfm-sobjs*))
    (dotimes (k 30) (%gfm-bump (aref v (mod k 3))))
    (let ((b0 (%gfm-bytes2)))
      (dotimes (k 30000) (%gfm-bump (aref v (mod k 3))))
      (< (/ (- (%gfm-bytes2) b0) 30000) 8)))
  t)
