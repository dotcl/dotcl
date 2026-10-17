;;; DOTCL-ROTATE-BYTE:ROTATE-BYTE, as sb-rotate-byte. A call on (BYTE 32 0) or
;;; (BYTE 64 0) with operands known to fit is compiled to a rotate instruction;
;;; each case is checked against the same call declared NOTINLINE and against a
;;; definition from LDB and DPB.

(require "dotcl-rotate-byte")

(defun %drb-ref (count size pos x)
  (if (zerop size)
      x
      (let ((c (mod count size)) (f (ldb (byte size pos) x)))
        (dpb (logior (ldb (byte size 0) (ash f c)) (ash f (- c size)))
             (byte size pos) x))))

(defun %drb-next (s) (ldb (byte 64 0) (+ (* s 6364136223846793005) 1442695040888963407)))

(defun %drb-r32 (c x)
  (declare (type (unsigned-byte 32) x) (type (integer -100 100) c))
  (dotcl-rotate-byte:rotate-byte c (byte 32 0) x))
(defun %drb-r64 (c x)
  (declare (type (unsigned-byte 64) x) (type (integer -100 100) c))
  (dotcl-rotate-byte:rotate-byte c (byte 64 0) x))
(defun %drb-r64-any (c x)
  (declare (type fixnum c))
  (dotcl-rotate-byte:rotate-byte c (byte 64 0) x))
(defun %drb-r32-slow (c x)
  (declare (notinline dotcl-rotate-byte:rotate-byte))
  (dotcl-rotate-byte:rotate-byte c (byte 32 0) x))
(declaim (inline %drb-permute))
(defun %drb-permute (data selector)
  (declare (type (unsigned-byte 32) data) (type (unsigned-byte 5) selector))
  (dotcl-rotate-byte:rotate-byte selector (byte 32 0) data))
(defun %drb-mix (state)
  (declare (type (unsigned-byte 64) state))
  (%drb-permute (ldb (byte 32 0) (logxor (ash state -18) state))
                (ldb (byte 5 59) state)))

(deftest dotcl-rotate-byte.examples
  (list (dotcl-rotate-byte:rotate-byte 3 (byte 32 0) #x80000001)
        (dotcl-rotate-byte:rotate-byte -1 (byte 32 0) 1)
        (dotcl-rotate-byte:rotate-byte 4 (byte 8 4) #xFFAB)
        (dotcl-rotate-byte:rotate-byte 1 (byte 0 3) -5)
        (dotcl-rotate-byte:rotate-byte 1 (byte 4 0) -2)
        (%drb-r64 1 #x8000000000000000)
        (%drb-r64-any 4 -1))
  (#xC #x80000000 #xFAFB -5 -3 1 -1))

(deftest dotcl-rotate-byte.general
  (let ((s 1) (bad '()))
    (dotimes (i 3000 (nreverse bad))
      (let* ((size (mod (setf s (%drb-next s)) 70))
             (pos (mod (setf s (%drb-next s)) 70))
             (x (- (* (setf s (%drb-next s)) (%drb-next s)) (ash 1 127)))
             (c (- (mod (setf s (%drb-next s)) 300) 150)))
        (unless (= (dotcl-rotate-byte:rotate-byte c (byte size pos) x)
                   (%drb-ref c size pos x))
          (push (list c size pos x) bad)))))
  nil)

(deftest dotcl-rotate-byte.words
  (let ((s 7) (bad '()))
    (dotimes (i 3000 (nreverse bad))
      (let ((c (- (mod (setf s (%drb-next s)) 200) 100))
            (x (setf s (%drb-next s)))
            (z (- (* (setf s (%drb-next s)) 65536) (ash 1 79))))
        (unless (= (%drb-r32 c (ldb (byte 32 0) x)) (%drb-ref c 32 0 (ldb (byte 32 0) x))
                   (%drb-r32-slow c (ldb (byte 32 0) x)))
          (push (list 32 c x) bad))
        (unless (= (%drb-r64 c x) (%drb-ref c 64 0 x))
          (push (list 64 c x) bad))
        (unless (= (%drb-r64-any c z) (%drb-ref c 64 0 z))
          (push (list :any c z) bad))
        (unless (= (%drb-mix x)
                   (%drb-ref (ldb (byte 5 59) x) 32 0 (ldb (byte 32 0) (logxor (ash x -18) x))))
          (push (list :mix x) bad)))))
  nil)

(deftest dotcl-rotate-byte.type-errors
  (list (handler-case (dotcl-rotate-byte:rotate-byte 1 (byte 64 0) 1.0) (type-error () :type-error))
        (handler-case (%drb-r64-any 1 1/2) (type-error () :type-error))
        (handler-case (dotcl-rotate-byte:rotate-byte 1.5 (byte 8 0) 1) (type-error () :type-error))
        (handler-case (dotcl-rotate-byte:rotate-byte 1 :x 1) (type-error () :type-error)))
  (:type-error :type-error :type-error :type-error))
