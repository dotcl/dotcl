;;; LIST and LIST* build their conses inline for a small argument count.
;;;
;;; Both used to pass their arguments to a runtime entry through an array, so a
;;; call cost 24 + 8n bytes on top of the conses it returned. Writing the same
;;; conses by hand already cost exactly the conses -- measured against SBCL,
;;; whose cons is 16 bytes to a .NET object's 32, (cons a (cons b nil)) sat
;;; exactly on that 2x floor while (list a b) was 40 bytes above it.
;;;
;;; What must not change: the answers, left-to-right argument order, LIST being
;;; shadowable, and the args-array path past the inline bound.

(deftest list-inline.shapes
  (list (list)
        (list 1)
        (list 1 2 3 4 5 6 7 8)
        (list 1 2 3 4 5 6 7 8 9))
  (nil (1) (1 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 8 9)))

(deftest list-inline.star-shapes
  (list (list* (list 1 2))
        (list* 1 (list 2 3))
        (list* 1 2 3)
        (list* 1 2 3 4 5 6 7 8 (list 9)))
  ((1 2) (1 2 3) (1 2 . 3) (1 2 3 4 5 6 7 8 9)))

;;; Only the primary value of an argument is taken.
(defun %lic-mv () (values 1 2 3))

(deftest list-inline.multiple-values-argument
  (list (list (%lic-mv) 9) (list 9 (%lic-mv)) (multiple-value-list (list 1 2)))
  ((1 9) (9 1) ((1 2))))

;;; Arguments are evaluated left to right, and exactly once each.
(defvar *lic-side* nil)
(defun %lic-note (x) (push x *lic-side*) x)

(deftest list-inline.evaluation-order
  (progn
    (setq *lic-side* nil)
    (let ((r (list (%lic-note :a) (%lic-note :b) (%lic-note :c))))
      (list r (reverse *lic-side*))))
  ((:a :b :c) (:a :b :c)))

(deftest list-inline.star-evaluation-order
  (progn
    (setq *lic-side* nil)
    (let ((r (list* (%lic-note :a) (%lic-note :b) (%lic-note :c))))
      (list r (reverse *lic-side*))))
  ((:a :b . :c) (:a :b :c)))

;;; A local function named LIST still shadows the operator.
(deftest list-inline.shadowed-by-flet
  (flet ((list (&rest a) (cons :shadow a)))
    (list 1 2))
  (:shadow 1 2))

(deftest list-inline.shadowed-by-labels
  (labels ((list (n) (if (<= n 0) :done (list (1- n)))))
    (list 2))
  :done)

;;; As a value, LIST is still the function.
(deftest list-inline.as-a-value
  (list (funcall #'list 1 2 3)
        (apply #'list 1 2 (list 3 4))
        (mapcar #'list (list 1 2) (list 3 4)))
  ((1 2 3) (1 2 3 4) ((1 3) (2 4))))

;;; Nesting, and a fresh list each call (not a shared constant).
(deftest list-inline.fresh-each-call
  (let ((a (list 1 2)) (b (list 1 2)))
    (list (equal a b) (eq a b) (progn (setf (car a) 9) (list a b))))
  (t nil ((9 2) (1 2))))

;;; --- the point: the conses, and nothing else ---

(defun %lic-bytes () (nth 4 (dotcl:gc-stats)))

(defmacro %lic-per-call (name &body body)
  `(progn
     (defun ,name (n)
       (declare (fixnum n))
       (let ((r nil))
         (do ((i 0 (1+ i))) ((= i n) r)
           (declare (fixnum i))
           (setq r (progn ,@body)))))
     (,name 2000)
     (let ((best nil))
       (dotimes (r 5 best)
         (let ((before (%lic-bytes)))
           (,name 100000)
           (let ((used (- (%lic-bytes) before)))
             (when (or (null best) (< used best)) (setq best used))))))))

(defvar *lic-a* 1)
(defvar *lic-b* 2)
(defvar *lic-c* 3)

;;; (LIST a b c) must cost the same as the conses written out by hand. Compared
;;; against that rather than against a constant, so the test says what it means
;;; even if a cons changes size. Within one byte per call, not equal: two runs of
;;; the same shape differ by a rounding's worth. What this guards against was
;;; 24 + 8n bytes per call.
(defun %lic-close-p (a b) (<= (abs (- a b)) 100000))

(deftest-compiled-only list-inline.costs-only-its-conses
  (list (%lic-close-p (%lic-per-call %lic-l3 (list *lic-a* *lic-b* *lic-c*))
                      (%lic-per-call %lic-c3 (cons *lic-a* (cons *lic-b* (cons *lic-c* nil)))))
        (%lic-close-p (%lic-per-call %lic-s3 (list* *lic-a* *lic-b* *lic-c*))
                      (%lic-per-call %lic-c2 (cons *lic-a* (cons *lic-b* *lic-c*)))))
  (t t))
