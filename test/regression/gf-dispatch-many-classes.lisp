;;; A generic function's dispatch cache holds the last few argument classes. It
;;; belongs to the generic function, not to a call site, so a program that
;;; makes instances of more classes than that, each with its own
;;; INITIALIZE-INSTANCE :AFTER method, recomputed the applicable methods on most
;;; MAKE-INSTANCE calls (cl-bench clos/instantiate rotates five classes). A
;;; per-class table behind the cache now keeps every class seen; it starts over
;;; when a method is added or removed or a class is (re)defined.

(defclass gfm-base () ((log :initform nil :accessor gfm-log)))
(defgeneric gfm-name (x))
(defmethod gfm-name ((x gfm-base)) :base)

(defmacro %gfm-define-classes (n)
  `(progn
     ,@(loop for i from 1 to n
             for name = (intern (format nil "GFM-C~d" i))
             collect `(defclass ,name (gfm-base) ())
             collect `(defmethod initialize-instance :after ((x ,name) &key)
                        (push ,i (gfm-log x)))
             when (evenp i)
               collect `(defmethod gfm-name ((x ,name)) ,i))))
(%gfm-define-classes 7)

(defvar *gfm-classes*
  (coerce (loop for i from 1 to 7 collect (find-class (intern (format nil "GFM-C~d" i)))) 'vector))

;; Every class, in rotation, more times than the front cache holds them.
(deftest gf-dispatch-many-classes.values
  (let ((out '()))
    (dotimes (k 21)
      (let ((x (make-instance (aref *gfm-classes* (mod k 7)))))
        (push (list (gfm-log x) (gfm-name x)) out)))
    (subseq (nreverse out) 14))
  (((1) :base) ((2) 2) ((3) :base) ((4) 4) ((5) :base) ((6) 6) ((7) :base)))

;; A method added after the table is warm is seen at once, and so is one removed.
(deftest gf-dispatch-many-classes.method-added-and-removed
  (let ((objs (map 'list #'make-instance *gfm-classes*)))
    (dotimes (k 3) (mapc #'gfm-name objs))
    (let ((m (defmethod gfm-name ((x gfm-c3)) :three)))
      (prog1 (list (mapcar #'gfm-name objs)
                   (progn (remove-method #'gfm-name m) (mapcar #'gfm-name objs)))
        nil)))
  ((:base 2 :three 4 :base 6 :base) (:base 2 :base 4 :base 6 :base)))

(defun %gfm-bytes () (nth 4 (dotcl:gc-stats)))

(deftest-compiled-only gf-dispatch-many-classes.no-recompute
  (let ((v *gfm-classes*))
    (dotimes (k 70) (make-instance (aref v (mod k 7))))
    (let ((b0 (%gfm-bytes)))
      (dotimes (k 7000) (make-instance (aref v (mod k 7))))
      (< (/ (- (%gfm-bytes) b0) 7000) 400)))
  t)
