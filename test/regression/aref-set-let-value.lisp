;;; (SETF (AREF A I) (LET (...) ... X)) with A and I plain variables (or
;;; literals) that the LET neither binds nor assigns stores X from inside the
;;; LET, where the bindings' types are known: an (unsigned-byte 32) value is
;;; stored raw instead of boxed and passed to the generic store. ironclad's DES
;;; key schedule writes each round key this way. When moving the store could
;;; change what is evaluated, it stays where it is.

(setf dotcl:*save-sil* t)

(defun %asl-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %asl-store (dst src i)
  (declare (type (simple-array (unsigned-byte 32) (*)) dst src)
           (type (integer 0 7) i))
  (setf (aref dst i)
        (let ((a (aref src i))
              (b (aref src (1+ i))))
          (logior (ash (mask-field (byte 8 0) a) 24) (ldb (byte 24 8) b)))))

;; The LET assigns the index: the index is read before the LET runs.
(defun %asl-assigns-index (v)
  (let ((i 0))
    (setf (aref v i) (let ((x 5)) (setq i 2) x))
    (list i (coerce v 'list))))

;; The LET binds the index's name: the outer I is the subscript.
(defun %asl-shadows-index (v)
  (let ((i 1))
    (setf (aref v i) (let ((i 3)) (* i 10)))
    (coerce v 'list)))

(defvar *asl-index* 0)
;; A special subscript is read before the LET rebinds it.
(defun %asl-special-index (v)
  (setf (aref v *asl-index*) (let ((*asl-index* 2)) (+ *asl-index* 40)))
  (coerce v 'list))

;; The array is a call: it runs before the LET.
(defun %asl-call-array (log)
  (let ((v (make-array 2 :initial-element 0)))
    (setf (aref (progn (push :array (car log)) v) 0)
          (let ((x 7)) (push :value (car log)) x))
    (list (reverse (car log)) (coerce v 'list))))

(defparameter *asl-src*
  (make-array 8 :element-type '(unsigned-byte 32)
                :initial-contents '(#xFFFFFFFF #x12345678 #x80000001 0 1 2 3 4)))

(deftest aref-set-let-value.values
  (let ((dst (make-array 8 :element-type '(unsigned-byte 32) :initial-element 0)))
    (list (%asl-store dst *asl-src* 0)
          (%asl-store dst *asl-src* 1)
          (aref dst 0) (aref dst 1)
          (%asl-assigns-index (make-array 3 :initial-element 0))
          (%asl-shadows-index (make-array 3 :initial-element 0))
          (%asl-special-index (make-array 3 :initial-element 0))
          (%asl-call-array (list nil))))
  (#xFF123456 #x78800000 #xFF123456 #x78800000
   (2 (5 0 0)) (0 30 0) (42 0 0) ((:array :value) (7 0))))

(deftest-emitting-only aref-set-let-value.stored-raw
  (let ((d (%asl-sil #'%asl-store)))
    (list (search "Runtime.ArefSetL)" d) (search "Runtime.ArefSet)" d)))
  (nil nil))
