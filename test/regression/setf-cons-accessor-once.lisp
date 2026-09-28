;;; SETF of FIRST, REST and the cXXr accessors evaluates the value form once,
;;; and the cons subform before the value form (CLHS 5.1.1.1).
;;;
;;; Bug: the expanders for FIRST, REST and CAAR..CDDDDR expanded to
;;; (progn (rplaca x value) value), so the value form ran twice. A value form
;;; that reads from a stream consumed two bytes per assignment: cl-jpeg reads
;;; the scan header with (setf (first (aref ...)) (read-jpeg-byte image)) and
;;; decoded every multi-component JPEG with a shifted header.

(defvar *setf-cons-count* 0)
(defun %setf-cons-next () (incf *setf-cons-count*))

(defmacro %setf-cons-run (&body body)
  `(let ((*setf-cons-count* 0))
     (list (progn ,@body) *setf-cons-count*)))

(deftest setf-cons-once.first-void
  (let ((l (list 0 0)))
    (%setf-cons-run (setf (first l) (%setf-cons-next)) l))
  ((1 0) 1))

(deftest setf-cons-once.first-value
  (let ((l (list 0 0)))
    (%setf-cons-run (setf (first l) (%setf-cons-next))))
  (1 1))

(deftest setf-cons-once.first-then-second
  (let ((v (vector (list 0 0))))
    (%setf-cons-run
      (setf (first (aref v 0)) (%setf-cons-next))
      (setf (second (aref v 0)) (%setf-cons-next))
      (aref v 0)))
  ((1 2) 2))

(deftest setf-cons-once.rest
  (let ((l (list 0 0)))
    (%setf-cons-run (setf (rest l) (list (%setf-cons-next))) l))
  ((0 1) 1))

(deftest setf-cons-once.cadr-cddr
  (let ((l (list 0 0 0)))
    (%setf-cons-run
      (setf (cadr l) (%setf-cons-next))
      (setf (cddr l) (list (%setf-cons-next)))
      l))
  ((0 1 2) 2))

(defun %setf-cons-caddr (l)
  (%setf-cons-run (setf (caddr l) (%setf-cons-next)) l))

(deftest setf-cons-once.caddr-in-defun
  (%setf-cons-caddr (list 0 0 0))
  ((0 0 1) 1))

(deftest setf-cons-once.subform-before-value
  ;; The cons subform is evaluated before the value form, so the store goes
  ;; into the list X held before the value form replaced it.
  (let* ((old (list 0 0))
         (x old))
    (setf (first x) (progn (setf x (list 9 9)) 1))
    (list old x))
  ((1 0) (9 9)))

(deftest setf-cons-once.second-subform-before-value
  (let* ((old (list 0 0))
         (x old))
    (setf (second x) (progn (setf x (list 9 9)) 1))
    (list old x))
  ((0 1) (9 9)))
