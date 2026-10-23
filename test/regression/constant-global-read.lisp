;;; A reference to a constant variable whose value is not folded (an array, a
;;; list) reads the global value directly: a constant cannot be bound, so the
;;; dynamic binding stack has nothing to say about it.

(setf dotcl:*save-sil* t)

(defconstant +cgr-table+ (if (boundp '+cgr-table+) (symbol-value '+cgr-table+)
                             (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(3 1 4 1))))
(defvar *cgr-special* 0)

(defun %cgr-sum ()
  (let ((s 0)) (dotimes (i 4 s) (incf s (aref +cgr-table+ i)))))

(defun %cgr-nested (n)
  (if (zerop n)
      (list (%cgr-sum) (eq +cgr-table+ (symbol-value '+cgr-table+)) *cgr-special*)
      (let ((*cgr-special* n)) (%cgr-nested (1- n)))))

(deftest constant-global-read.values
  (list (%cgr-sum) (%cgr-nested 20)
        (let ((*cgr-special* :x)) (list (%cgr-sum) *cgr-special*))
        (handler-case (eval '(let ((+cgr-table+ 1)) +cgr-table+)) (error () :error)))
  (9 (9 t 1) (9 :x) :error))

(deftest-emitting-only constant-global-read.direct
  (let ((text (%flat-text (dotcl:function-sil #'%cgr-sum))))
    (list (and (search "DynamicBindings.GetConstant" text) t)
          (and (search "DynamicBindings.Get)" text) t)))
  (t nil))

;;; A constant's value is fixed, so an AREF of a constant specialized array is
;;; compiled for that array's element type.

(defconstant +cgr-rot+ (if (boundp '+cgr-rot+) (symbol-value '+cgr-rot+)
                           (make-array 4 :element-type '(unsigned-byte 5) :initial-contents '(1 2 30 31))))
(defconstant +cgr-grid+ (if (boundp '+cgr-grid+) (symbol-value '+cgr-grid+)
                            (make-array '(2 2) :element-type '(signed-byte 16) :initial-contents '((-1 2) (3 -4)))))
(defconstant +cgr-gen+ (if (boundp '+cgr-gen+) (symbol-value '+cgr-gen+) (vector 1 :a 3)))

(defun %cgr-rot-sum (j)
  (let ((s 0)) (dotimes (i 4 s) (incf s (+ j (aref +cgr-rot+ i))))))

(deftest constant-global-read.typed-aref
  (list (%cgr-rot-sum 10) (aref +cgr-grid+ 1 1) (+ (aref +cgr-grid+ 0 0) (aref +cgr-grid+ 1 0))
        (aref +cgr-gen+ 1)
        (handler-case (aref +cgr-rot+ (if (zerop (length +cgr-gen+)) 0 4)) (error () :error)))
  (104 -4 2 :a :error))

(deftest-emitting-only constant-global-read.typed-aref-shape
  (let ((text (%flat-text (dotcl:function-sil #'%cgr-rot-sum))))
    (list (and (search "Runtime.ArefL)" text) t) (and (search "(LDELEM-" text) t)))
  (nil t))
