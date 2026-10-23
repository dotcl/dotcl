;;; Elements of specialized vectors read and written through each path the
;;; compiler picks: a constant variable (the per-element helper), a declared
;;; parameter (the hoisted storage), and a call through #'AREF. Every path has
;;; to see the same storage for every element type.

(defmacro %tvsp-define (name type values)
  (let ((const (intern (format nil "+~A+" name)))
        (decl (intern (format nil "~A-DECL" name)))
        (decl-set (intern (format nil "~A-DECL-SET" name))))
    `(progn
       (defconstant ,const
         (if (boundp ',const)
             (symbol-value ',const)
             (make-array ,(length values) :element-type ',type
                                          :initial-contents ',values)))
       (defun ,name (i) (aref ,const i))
       (defun ,decl (v i)
         (declare (type (simple-array ,type (*)) v))
         (aref v i))
       (defun ,decl-set (v i x)
         (declare (type (simple-array ,type (*)) v))
         (setf (aref v i) x)))))

(%tvsp-define %tvsp-u8 (unsigned-byte 8) (0 1 127 128 255))
(%tvsp-define %tvsp-u16 (unsigned-byte 16) (0 1 255 256 65535))
(%tvsp-define %tvsp-u32 (unsigned-byte 32) (0 1 65536 2147483648 4294967295))
(%tvsp-define %tvsp-u64 (unsigned-byte 64) (0 1 4294967296 9223372036854775808 18446744073709551615))
(%tvsp-define %tvsp-s8 (signed-byte 8) (-128 -1 0 1 127))
(%tvsp-define %tvsp-s16 (signed-byte 16) (-32768 -1 0 1 32767))
(%tvsp-define %tvsp-s32 (signed-byte 32) (-2147483648 -1 0 1 2147483647))
(%tvsp-define %tvsp-s64 (signed-byte 64) (-9223372036854775808 -1 0 1 9223372036854775807))
(%tvsp-define %tvsp-fix fixnum (-4611686018427387904 -1 0 1 4611686018427387903))
(%tvsp-define %tvsp-sf single-float (-1.5f0 0f0 0.25f0 1f10 3f0))
(%tvsp-define %tvsp-df double-float (-1.5d0 0d0 0.25d0 1d300 3d0))
(%tvsp-define %tvsp-ch character (#\a #\Z #\0 #\Space #\~))

(deftest typed-vector-storage-paths.read
  (let ((bad '()))
    (dolist (e (list (list #'%tvsp-u8 #'%tvsp-u8-decl +%tvsp-u8+)
                     (list #'%tvsp-u16 #'%tvsp-u16-decl +%tvsp-u16+)
                     (list #'%tvsp-u32 #'%tvsp-u32-decl +%tvsp-u32+)
                     (list #'%tvsp-u64 #'%tvsp-u64-decl +%tvsp-u64+)
                     (list #'%tvsp-s8 #'%tvsp-s8-decl +%tvsp-s8+)
                     (list #'%tvsp-s16 #'%tvsp-s16-decl +%tvsp-s16+)
                     (list #'%tvsp-s32 #'%tvsp-s32-decl +%tvsp-s32+)
                     (list #'%tvsp-s64 #'%tvsp-s64-decl +%tvsp-s64+)
                     (list #'%tvsp-fix #'%tvsp-fix-decl +%tvsp-fix+)
                     (list #'%tvsp-sf #'%tvsp-sf-decl +%tvsp-sf+)
                     (list #'%tvsp-df #'%tvsp-df-decl +%tvsp-df+)
                     (list #'%tvsp-ch #'%tvsp-ch-decl +%tvsp-ch+)))
      (destructuring-bind (by-const by-decl v) e
        (dotimes (i (length v))
          (let ((g (funcall #'aref v i)))
            (unless (and (eql g (funcall by-const i))
                         (eql g (funcall by-decl v i)))
              (push (list (array-element-type v) i) bad))))))
    bad)
  nil)

(deftest typed-vector-storage-paths.write
  (let ((bad '()))
    (dolist (e (list (list #'%tvsp-u8-decl-set '(unsigned-byte 8) '(0 200 255))
                     (list #'%tvsp-u16-decl-set '(unsigned-byte 16) '(0 300 65535))
                     (list #'%tvsp-u32-decl-set '(unsigned-byte 32) '(0 70000 4294967295))
                     (list #'%tvsp-u64-decl-set '(unsigned-byte 64) '(0 18446744073709551615))
                     (list #'%tvsp-s8-decl-set '(signed-byte 8) '(-128 0 127))
                     (list #'%tvsp-s16-decl-set '(signed-byte 16) '(-32768 0 32767))
                     (list #'%tvsp-s32-decl-set '(signed-byte 32) '(-2147483648 0 2147483647))
                     (list #'%tvsp-s64-decl-set '(signed-byte 64) '(-9223372036854775808 9223372036854775807))
                     (list #'%tvsp-fix-decl-set 'fixnum '(-5 0 5))
                     (list #'%tvsp-sf-decl-set 'single-float '(-2.5f0 7f0))
                     (list #'%tvsp-df-decl-set 'double-float '(-2.5d0 7d0))
                     (list #'%tvsp-ch-decl-set 'character '(#\x #\Y))))
      (destructuring-bind (setter type xs) e
        (let ((v (make-array (length xs) :element-type type)))
          (loop for x in xs for i from 0 do (funcall setter v i x))
          (unless (equal (coerce v 'list) xs)
            (push type bad)))))
    bad)
  nil)

(deftest typed-vector-storage-paths.store-range
  (let ((v (make-array 2 :element-type '(unsigned-byte 8))))
    (list (handler-case (progn (%tvsp-u8-decl-set v 0 256) :stored)
            (error () :error))
          (aref v 0)))
  (:error 0))
