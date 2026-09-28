;;; The array and sequence element readers answer exactly one value, and the
;;; compiler knows it.
;;;
;;; SVREF, SCHAR, BIT, SBIT, ROW-MAJOR-AREF and ELT each return the one object
;;; stored at the index (CLHS gives each of them a single ELEMENT value). That
;;; object is never an MvReturn: every store into an element takes its value in
;;; a single-value position, the same reason AREF and CHAR were already on the
;;; single-value list. So the Runtime.UnwrapMv that used to follow these reads
;;; wherever a value was consumed (a SETQ, an argument) could only be a no-op,
;;; and it is no longer emitted.
;;;
;;; A local function that takes one of these names is a call to that function
;;; and may answer several values, so it keeps its unwrap. The value tests come
;;; first and the instruction counts after.

(setf dotcl:*save-sil* t)

(defun %ersv-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %ersv-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the shapes: each result is consumed by a SETQ, not returned ----

(defun %ersv-svref (v i) (let ((x nil)) (setq x (svref v i)) (list x)))
(defun %ersv-schar (s i) (let ((x nil)) (setq x (schar s i)) (list x)))
(defun %ersv-bit (b i) (let ((x nil)) (setq x (bit b i)) (list x)))
(defun %ersv-sbit (b i) (let ((x nil)) (setq x (sbit b i)) (list x)))
(defun %ersv-rma (a i) (let ((x nil)) (setq x (row-major-aref a i)) (list x)))
(defun %ersv-elt (q i) (let ((x nil)) (setq x (elt q i)) (list x)))

;; The control: a call that is not known to be single-valued keeps its unwrap.
;; If this ever reads 0, the counts below prove nothing.
(defun %ersv-other (v i) (let ((x nil)) (setq x (identity (svref v i))) (list x)))

;; A local function under the reader's name.
(defun %ersv-flet (v i)
  (flet ((svref (a b) (declare (ignore a b)) (values 11 22)))
    (let ((x nil))
      (setq x (svref v i))
      (list x (multiple-value-list (svref v i))))))

;;; ---- values ----

(deftest element-reader-single-value.values
  (list (%ersv-svref (vector 1 2 3) 1)
        (%ersv-schar (copy-seq "abc") 2)
        (%ersv-bit (make-array 3 :element-type 'bit :initial-contents '(0 1 0)) 1)
        (%ersv-sbit (make-array 3 :element-type 'bit :initial-contents '(1 0 1)) 1)
        (%ersv-rma (make-array '(2 2) :initial-contents '((a b) (c d))) 2)
        (%ersv-elt (list 'p 'q 'r) 2)
        (%ersv-elt (vector 'p 'q 'r) 0)
        (%ersv-other (vector 4 5) 0))
  ((2) (#\c) (1) (0) (c) (r) (p) (4)))

(deftest element-reader-single-value.one-value-each
  (let ((v (vector 1 2 3))
        (s (copy-seq "abc"))
        (b (make-array 2 :element-type 'bit :initial-contents '(0 1)))
        (a (make-array '(2 2) :initial-contents '((a b) (c d)))))
    (list (multiple-value-list (svref v 0))
          (multiple-value-list (schar s 0))
          (multiple-value-list (bit b 1))
          (multiple-value-list (sbit b 1))
          (multiple-value-list (row-major-aref a 3))
          (multiple-value-list (elt v 2))))
  ((1) (#\a) (1) (1) (d) (3)))

;; An element holds the primary of a multi-valued store, and reading it back
;; gives that one object.
(deftest element-reader-single-value.element-holds-primary
  (let ((v (vector 0 0)))
    (setf (svref v 0) (values 61 62))
    (setf (elt v 1) (values 71 72))
    (list (multiple-value-list (svref v 0))
          (multiple-value-list (elt v 1))))
  ((61) (71)))

(deftest element-reader-single-value.flet-shadow-keeps-values
  (%ersv-flet (vector 1 2) 0)
  (11 (11 22)))

;;; ---- emitted code ----

(deftest-emitting-only element-reader-single-value.no-unwrap
  (mapcar (lambda (fn) (%ersv-count "Runtime.UnwrapMv" (%ersv-sil fn)))
          (list #'%ersv-svref #'%ersv-schar #'%ersv-bit #'%ersv-sbit
                #'%ersv-rma #'%ersv-elt))
  (0 0 0 0 0 0))

(deftest-emitting-only element-reader-single-value.control-keeps-unwrap
  (plusp (%ersv-count "Runtime.UnwrapMv" (%ersv-sil #'%ersv-other)))
  t)

(deftest-emitting-only element-reader-single-value.flet-shadow-keeps-unwrap
  (plusp (%ersv-count "Runtime.UnwrapMv" (%ersv-sil #'%ersv-flet)))
  t)
