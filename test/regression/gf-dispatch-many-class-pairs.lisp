;;; The per-class table behind a generic function's dispatch cache kept only
;;; entries made for one dispatch class. A two-argument generic function sees a
;;; class pair per call, and with more pairs in rotation than the front cache
;;; holds, every call recomputed the applicable methods and sorted them again.
;;; Coalton's type checker (APPLY-SUBSTITUTION on a substitution list and a
;;; type, TY= on two types) does this, and spent most of its time collecting the
;;; garbage that made. Entries for two or more classes now have a table of their
;;; own, keyed by the whole class vector.

(defclass gfp-base () ())
(defgeneric gfp-tag (list x))
(defmethod gfp-tag (list (x gfp-base)) (list (if list :cons :null) :base))

(defmacro %gfp-define-classes (n)
  `(progn
     ,@(loop for i from 1 to n
             for name = (intern (format nil "GFP-C~d" i))
             collect `(defclass ,name (gfp-base) ())
             when (evenp i)
               collect `(defmethod gfp-tag ((list cons) (x ,name)) (list :cons ,i)))))
(%gfp-define-classes 7)

(defvar *gfp-objects*
  (coerce (loop for i from 1 to 7
                collect (make-instance (intern (format nil "GFP-C~d" i))))
          'vector))

;; Fourteen class pairs in rotation, more than the front cache holds.
(deftest gf-dispatch-many-class-pairs.values
  (let ((out '()))
    (dotimes (k 42)
      (push (gfp-tag (if (evenp (floor k 7)) nil (list 1)) (aref *gfp-objects* (mod k 7))) out))
    (subseq (nreverse out) 28))
  ((:null :base) (:null :base) (:null :base) (:null :base) (:null :base) (:null :base) (:null :base)
   (:cons :base) (:cons 2) (:cons :base) (:cons 4) (:cons :base) (:cons 6) (:cons :base)))

;; A method added after the table is warm is seen at once, and so is one removed.
(deftest gf-dispatch-many-class-pairs.method-added-and-removed
  (let ((objs (coerce *gfp-objects* 'list)))
    (dotimes (k 3) (dolist (o objs) (gfp-tag nil o) (gfp-tag '(1) o)))
    (let ((m (defmethod gfp-tag ((list null) (x gfp-c3)) :three)))
      (prog1 (list (mapcar (lambda (o) (gfp-tag nil o)) objs)
                   (progn (remove-method #'gfp-tag m)
                          (mapcar (lambda (o) (gfp-tag nil o)) objs)))
        nil)))
  ((#1=(:null :base) #1# :three #1# #1# #1# #1#)
   (#1# #1# #1# #1# #1# #1# #1#)))

(defun %gfp-bytes () (nth 4 (dotcl:gc-stats)))

(deftest-compiled-only gf-dispatch-many-class-pairs.no-recompute
  (let ((v *gfp-objects*) (lists (vector nil (list 1))))
    (dotimes (k 140) (gfp-tag (aref lists (mod (floor k 7) 2)) (aref v (mod k 7))))
    (let ((b0 (%gfp-bytes)))
      (dotimes (k 14000) (gfp-tag (aref lists (mod (floor k 7) 2)) (aref v (mod k 7))))
      (< (/ (- (%gfp-bytes) b0) 14000) 200)))
  t)
