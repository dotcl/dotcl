;;; A class redefinition that moves an accessor's slot to another index. The
;;; generic function's dispatch cache keeps a shortcut that reads or writes the
;;; slot by index; it kept the old index, so after the redefinition the reader
;;; read, and the writer wrote, whichever slot now has that index.
;;; (map 'list #'accessor objs) before and after the redefinition read the slot
;;; that took the old place (SBCL 2.6.8 reads the moved slot).
;;;
;;; Covered: the shortcut in the recent-classes cache, the same shortcut in the
;;; per-class table behind it (more classes than the recent cache holds), and
;;; the call-site cache of an accessor used in a function on several classes.

(defclass aso-base () ((n :initform 0 :accessor aso-n)))
(defmacro %aso-classes (family k)
  `(progn ,@(loop for i from 1 to k
                  collect `(defclass ,(intern (format nil "ASO-~a~d" family i)) (aso-base)
                             ((a :initform ,i))))))
(%aso-classes "P" 2)
(%aso-classes "Q" 6)
(defun %aso-objs (family k)
  (loop for i from 1 to k collect (make-instance (intern (format nil "ASO-~a~d" family i)))))
(defun %aso-bump (x) (incf (aso-n x)))
;; The generic functions as values, so the calls below go through them rather
;; than through a compiled accessor call site.
(defvar *aso-rd* #'aso-n)
(defvar *aso-wr* #'(setf aso-n))
(defun %aso-gf-bump (o) (funcall *aso-wr* (+ (funcall *aso-rd* o) 1) o))

(defun %aso-run (family k redefined)
  (let ((objs (%aso-objs family k)))
    ;; Through the generic functions, and through the compiled call site in
    ;; %ASO-BUMP, a few times round.
    (dotimes (r 3)
      (mapc #'%aso-gf-bump objs)
      (mapc #'%aso-bump objs))
    (eval `(defclass ,redefined (aso-base) ((z :initform :z) (a :initform :a))))
    (dotimes (r 2)
      (mapc #'%aso-gf-bump objs)
      (mapc #'%aso-bump objs))
    (list (mapcar *aso-rd* objs)
          (mapcar (lambda (o) (slot-value o 'a)) objs))))

(deftest accessor-after-slot-reorder.recent-cache
  (%aso-run "P" 2 'aso-p1)
  ((10 10) (1 2)))

(deftest accessor-after-slot-reorder.per-class-table
  (%aso-run "Q" 6 'aso-q5)
  ((10 10 10 10 10 10) (1 2 3 4 5 6)))
