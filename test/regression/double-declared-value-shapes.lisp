;;; A DOUBLE-FLOAT declaration reaches the value through the wrappers generated
;;; code puts around it: (THE (VALUES DOUBLE-FLOAT &OPTIONAL) (LET ... (VALUES
;;; (PROGN (SQRT X))))), a symbol macro standing for a double, (OR (IGNORE-ERRORS
;;; (COERCE I 'DOUBLE-FLOAT)) ...), and a COND whose arms are all doubles.
;;; The value is computed as a raw double, not through the generic SQRT, COERCE
;;; and arithmetic.

(setf dotcl:*save-sil* t)

(defvar *ddv-cell* (cons 7.0d0 nil))
(define-symbol-macro ddv-nan (the double-float (car *ddv-cell*)))

(declaim (ftype (function (double-float) (values double-float &optional))
                %ddv-root %ddv-root-1 %ddv-guarded %ddv-from-int))

(defun %ddv-root (d)
  (locally (declare (optimize (safety 0)))
    (the (values double-float &optional) (let ((x d)) (values (progn (sqrt x)))))))

(defun %ddv-root-1 (d)
  (locally (declare (optimize (safety 1)))
    (the (values double-float &optional) (let ((x d)) (values (progn (sqrt x)))))))

(defun %ddv-guarded (d)
  (let ((root (if (< d 0.0d0)
                  ddv-nan
                  (the (values double-float &optional) (let ((x d)) (values (sqrt x)))))))
    (+ root 1.0d0)))

(defun %ddv-from-int (d)
  (the (values double-float &optional)
       (let ((x 3))
         (values (- (or (ignore-errors (coerce x 'double-float))
                        (if (< x 0) -1.0d0 1.0d0))
                    d)))))

(declaim (ftype (function (double-float double-float) (values double-float &optional)) %ddv-add))
(defun %ddv-add (a b) (+ a b))

(defun %ddv-cond (o d)
  (declare (type double-float d))
  (let ((limit (cond ((null o) d)
                     ((consp o) (the double-float (car o)))
                     (t 0.5d0))))
    (%ddv-add limit 1.0d0)))

(deftest double-declared-value-shapes.values
  (list (%ddv-root 4.0d0) (%ddv-root-1 9.0d0) (%ddv-guarded 16.0d0) (%ddv-guarded -1.0d0)
        (%ddv-from-int 0.5d0) (%ddv-cond nil 2.0d0) (%ddv-cond (list 3.0d0) 0.0d0)
        (%ddv-cond :x 0.0d0))
  (2.0d0 3.0d0 5.0d0 8.0d0 2.5d0 3.0d0 4.0d0 1.5d0))

(deftest-compiled-only double-declared-value-shapes.negative-root-is-a-type-error
  (handler-case (progn (%ddv-root-1 -4.0d0) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest double-declared-value-shapes.ignore-errors-still-catches
  (list (ignore-errors (coerce 'a 'double-float))
        (let ((i 2)) (ignore-errors (coerce i 'double-float)))
        (handler-case (car '(1)) (error () :e)))
  (nil 2.0d0 1))

(deftest-compiled-only double-declared-value-shapes.raw
  (let ((root (%flat-text (dotcl:function-sil #'%ddv-root)))
        (guarded (%flat-text (dotcl:function-sil #'%ddv-guarded)))
        (from-int (%flat-text (dotcl:function-sil #'%ddv-from-int)))
        (c (%flat-text (dotcl:function-sil #'%ddv-cond))))
    (list (search "SQRT" root) (search "SQRT" guarded) (search "Runtime.Add" guarded)
          (search "Coerce" from-int) (search "PushCluster" from-int)
          (let ((at (search "LIMIT" c)))
            (and at (search "Double" c :start2 at :end2 (min (length c) (+ at 16))) t))))
  (nil nil nil nil nil t))

(setf dotcl:*save-sil* nil)
