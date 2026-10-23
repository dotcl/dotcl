;;; A loop counter declared an integer type, stepped by the shape
;;; (THE <type> (LET ((B 1)) (LET ((A I) (B B)) (+ A B)))) -- an integer
;;; constant bound to a variable and a THE around the step -- is stepped as a
;;; raw int64 with an overflow check, not through a boxed generic addition; and
;;; THE still checks the value against its type.

(setf dotcl:*save-sil* t)

(deftype %ttic-ufix () '(and fixnum unsigned-byte))

(defun %ttic-count (n)
  (declare (type %ttic-ufix n))
  (labels ((rec (i acc)
             (declare (type %ttic-ufix i acc))
             (if (= i n)
                 acc
                 (rec (the (values %ttic-ufix &optional)
                           (let ((b 1)) (let ((a i) (b b)) (+ a b))))
                      (the %ttic-ufix (let ((a acc) (c 2)) (+ a c)))))))
    (rec 0 0)))

(defun %ttic-narrow (n)
  (declare (fixnum n))
  (labels ((rec (i)
             (declare (fixnum i))
             (if (> i 1000)
                 i
                 (rec (the (integer 0 10) (let ((b 3)) (+ i b)))))))
    (rec n)))

(deftest tco-the-int-const.values
  (list (%ttic-count 0) (%ttic-count 10) (%ttic-count 100000))
  (0 20 200000))

;; The emit-free interpreter checks THE only against plain FIXNUM.
(deftest-compiled-only tco-the-int-const.the-checks
  (handler-case (%ttic-narrow 0) (type-error () :type-error))
  :type-error)

(deftest-compiled-only tco-the-int-const.raw
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "Runtime.AddFixnum)" #'%ttic-count) (has "AddFixnumChecked" #'%ttic-count)))
  (nil t))

(setf dotcl:*save-sil* nil)
