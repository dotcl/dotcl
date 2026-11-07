;;; DOTCL:MAKE-PINNED-VECTOR makes an ordinary specialized vector whose storage
;;; the GC does not move, and DOTCL:PINNED-VECTOR-ADDRESS returns the address of
;;; its first element. This is what the static-vectors library needs: AREF on
;;; the Lisp side and a stable foreign pointer on the C side, over one buffer.

(deftest pinned-vector.is-a-simple-specialized-vector
  (let ((v (dotcl:make-pinned-vector 4 '(unsigned-byte 8))))
    (list (length v)
          (typep v '(simple-array (unsigned-byte 8) (4)))
          (equal (array-element-type v) '(unsigned-byte 8))
          (every #'zerop v)))
  (4 t t t))

(deftest pinned-vector.aref-and-memory-agree
  (let* ((v (dotcl:make-pinned-vector 4 '(unsigned-byte 8)))
         (a (dotcl:pinned-vector-address v)))
    (setf (aref v 1) 200)
    (dotnet:mem-write 77 :uint8 a 2)
    (prog1 (list (dotnet:mem-read :uint8 a 1) (coerce v 'list))
      (dotcl:unpin-vector v)))
  (200 (0 200 77 0)))

(deftest pinned-vector.element-widths
  (let ((d (dotcl:make-pinned-vector 3 'double-float))
        (i (dotcl:make-pinned-vector 3 '(signed-byte 32)))
        (u (dotcl:make-pinned-vector 3 '(unsigned-byte 16))))
    (dotnet:mem-write 2.5d0 :double (dotcl:pinned-vector-address d) 8)
    (fill i -5)
    (setf (aref u 2) 65535)
    (list (aref d 1)
          (dotnet:mem-read :int (dotcl:pinned-vector-address i) 4)
          (dotnet:mem-read :uint16 (dotcl:pinned-vector-address u) 4)))
  (2.5d0 -5 65535))

(deftest pinned-vector.address-survives-gc
  (let* ((v (dotcl:make-pinned-vector 64 '(unsigned-byte 8)))
         (a (dotcl:pinned-vector-address v)))
    (dotimes (i 3)
      (make-list 10000)
      (dotnet:static "System.GC" "Collect"))
    (list (= a (dotcl:pinned-vector-address v))
          (dotcl:unpin-vector v)))
  (t t))

(deftest pinned-vector.unpinned-has-no-address
  (let ((v (dotcl:make-pinned-vector 2 '(unsigned-byte 8))))
    (list (dotcl:unpin-vector v)
          (dotcl:unpin-vector v)
          (dotcl:unpin-vector (make-array 2 :element-type '(unsigned-byte 8)))
          (handler-case (progn (dotcl:pinned-vector-address v) :no-error)
            (type-error () :type-error))
          ;; The vector itself stays a usable Lisp vector.
          (progn (setf (aref v 0) 9) (aref v 0))))
  (t nil nil :type-error 9))

;;; MAKE-ARRAY keeps (unsigned-byte 32) in 64-bit slots and (signed-byte 8/16)
;;; in 32-bit ones. A pinned vector must use the width foreign code expects,
;;; and AREF, declared access and sequence functions must still work on it.
(defun pinned-vector-u32-set (v)
  (declare (type (simple-array (unsigned-byte 32) (*)) v))
  (setf (aref v 0) 4000000000)
  (aref v 0))

(defun pinned-vector-s8-sum (v)
  (declare (type (simple-array (signed-byte 8) (*)) v) (optimize (safety 0)))
  (setf (aref v 1) -100)
  (loop for x across v sum x))

(deftest pinned-vector.foreign-widths
  (let ((u (dotcl:make-pinned-vector 3 '(unsigned-byte 32)))
        (s (dotcl:make-pinned-vector 3 '(signed-byte 8)))
        (h (dotcl:make-pinned-vector 3 '(signed-byte 16))))
    (setf (aref h 2) -30000)
    (list (pinned-vector-u32-set u)
          (dotnet:mem-read :uint32 (dotcl:pinned-vector-address u) 0)
          (dotnet:mem-read :uint32 (dotcl:pinned-vector-address u) 4)
          (pinned-vector-s8-sum s)
          (dotnet:mem-read :int8 (dotcl:pinned-vector-address s) 1)
          (dotnet:mem-read :int16 (dotcl:pinned-vector-address h) 4)
          (equal (array-element-type u) '(unsigned-byte 32))
          (coerce (replace u '(1 2 3)) 'list)
          (coerce (subseq s 1) 'list)
          (handler-case (progn (setf (aref s 0) 200) :stored)
            (type-error () :type-error))))
  (4000000000 4000000000 0 -100 -100 -30000 t (1 2 3) (-100 0) :type-error))

(deftest pinned-vector.unsupported-element-types-signal
  ;; T has no raw fixed-width storage: refuse rather than hand out an address
  ;; with the wrong layout. (unsigned-byte 64) is kept as its bits, 8 bytes
  ;; each, like (signed-byte 64), which FIXNUM is.
  (mapcar (lambda (et)
            (handler-case (progn (dotcl:make-pinned-vector 3 et) :made)
              (error () :error)))
          '((unsigned-byte 64) t fixnum (unsigned-byte 8)))
  (:made :error :made :made))

(deftest pinned-vector.pinned-vector-p
  ;; A pinned vector is simple, so ADJUST-ARRAY gives back a fresh vector
  ;; with ordinary storage and leaves the pinned one as it was.
  (let* ((v (dotcl:make-pinned-vector 4 '(unsigned-byte 8)))
         (w (dotcl:make-pinned-vector 4 'double-float))
         (w2 (adjust-array w 16)))
    (list (dotcl:pinned-vector-p v)
          (progn (dotcl:unpin-vector v) (dotcl:pinned-vector-p v))
          (dotcl:pinned-vector-p (make-array 4 :element-type '(unsigned-byte 8)))
          (dotcl:pinned-vector-p (vector 1 2))
          (dotcl:pinned-vector-p 42)
          (eq w w2)
          (dotcl:pinned-vector-p w2)
          (handler-case (progn (dotcl:pinned-vector-address w2) :no-error)
            (error () :error))
          (prog1 (dotcl:pinned-vector-p w) (dotcl:unpin-vector w))))
  (t nil nil nil nil nil nil :error t))

;;; :ALIGNMENT puts element 0 at a multiple of that many bytes, for every supported element width,
;;; and a vector made without it is made as before.

(defun %pv-aligned-p (v alignment)
  (zerop (mod (dotcl:pinned-vector-address v) alignment)))

(deftest pinned-vector.alignment-16-64-4096
  (let ((vs (loop for al in '(16 64 4096)
                  collect (list al (dotcl:make-pinned-vector 100 '(unsigned-byte 8) :alignment al)))))
    (prog1 (mapcar (lambda (e) (%pv-aligned-p (second e) (first e))) vs)
      (dolist (e vs) (dotcl:unpin-vector (second e)))))
  (t t t))

(deftest pinned-vector.alignment-for-each-element-type
  (let ((made (loop for et in '((unsigned-byte 8) (signed-byte 8) (unsigned-byte 16) (signed-byte 32)
                                (unsigned-byte 64) single-float double-float)
                    collect (dotcl:make-pinned-vector 37 et :alignment 64))))
    (prog1 (list (every (lambda (v) (%pv-aligned-p v 64)) made)
                 (mapcar #'length made)
                 (typep (first made) '(simple-array (unsigned-byte 8) (37))))
      (mapc #'dotcl:unpin-vector made)))
  (t (37 37 37 37 37 37 37) t))

(deftest pinned-vector.alignment-vector-still-works
  (let ((v (dotcl:make-pinned-vector 8 '(unsigned-byte 8) :alignment 4096)))
    (setf (aref v 3) 9)
    (dotnet:mem-write 7 :uint8 (dotcl:pinned-vector-address v) 5)
    (prog1 (list (coerce v 'list) (dotcl:pinned-vector-p v))
      (dotcl:unpin-vector v)))
  ((0 0 0 9 0 7 0 0) t))

(deftest pinned-vector.alignment-reuse-after-unpin
  (let ((ok t))
    (dotimes (i 20)
      (let ((v (dotcl:make-pinned-vector (+ 10 i) '(unsigned-byte 32) :alignment 64)))
        (unless (%pv-aligned-p v 64) (setf ok nil))
        (dotcl:unpin-vector v)))
    (setf ok (and ok (zerop (length (dotcl:make-pinned-vector 0 '(unsigned-byte 8) :alignment 64)))))
    ok)
  t)

(deftest pinned-vector.alignment-1-and-none-agree
  (list (length (dotcl:make-pinned-vector 5 '(unsigned-byte 8) :alignment 1))
        (length (dotcl:make-pinned-vector 5 '(unsigned-byte 8))))
  (5 5))

(deftest pinned-vector.alignment-rejects-bad-values
  (mapcar (lambda (al)
            (handler-case (progn (dotcl:make-pinned-vector 4 '(unsigned-byte 8) :alignment al) :accepted)
              (error () :error)))
          '(3 0 -8 131072 :x nil))
  (:error :error :error :error :error :error))

(deftest pinned-vector.alignment-large-vectors
  (let ((vs (list (dotcl:make-pinned-vector 200000 '(unsigned-byte 8) :alignment 4096)
                  (dotcl:make-pinned-vector 1000000 'double-float :alignment 64)
                  (dotcl:make-pinned-vector 300000 '(unsigned-byte 32) :alignment 16))))
    (prog1 (list (%pv-aligned-p (first vs) 4096) (%pv-aligned-p (second vs) 64) (%pv-aligned-p (third vs) 16))
      (mapc #'dotcl:unpin-vector vs)))
  (t t t))
