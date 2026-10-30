;;; DOTCL-SIMD buffer reads and writes at (safety 0) trust the declaration.

(require "dotcl-simd")

(setf dotcl:*save-sil* t)

(defparameter *snc-kinds*
  '((double-float "F64.2")
    (single-float "F32.4")
    ((signed-byte 64) "S64.2")
    ((unsigned-byte 64) "U64.2")
    ((signed-byte 32) "S32.4")
    ((unsigned-byte 16) "U16.8")
    ((unsigned-byte 8) "U8.16")))

(defun %snc-lane-name (kind)
  (second (assoc kind '(("F64.2" "Double") ("F32.4" "Single")
                        ("S64.2" "Int64") ("U64.2" "UInt64")
                        ("S32.4" "Int32") ("U16.8" "UInt16")
                        ("U8.16" "Byte"))
                   :test #'string=)))

(defun %snc-lane-count (kind)
  (cdr (assoc kind '(("F64.2" . 2) ("F32.4" . 4) ("S64.2" . 2) ("U64.2" . 2)
                     ("S32.4" . 4) ("U16.8" . 8) ("U8.16" . 16))
              :test #'string=)))

(defun %snc-name (kind op)
  (intern (format nil "%SNC-~A-~A" op kind) "COMMON-LISP-USER"))

(defun %snc-aref (kind)
  (intern (concatenate 'string kind "-AREF") "DOTCL-SIMD"))

(defun %snc-read-form (type kind)
  `(lambda (a i)
     (declare (type (simple-array ,type (*)) a) (fixnum i)
              (optimize (safety 0)))
     (loop repeat 1 do (aref a 0))
     (load-time-value 0)
     (,(%snc-aref kind) a i)))

(defun %snc-write-form (type kind)
  `(lambda (a i v)
     (declare (type (simple-array ,type (*)) a) (fixnum i)
              (optimize (safety 0)))
     (loop repeat 1 do (aref a 0))
     (load-time-value 0)
     (setf (,(%snc-aref kind) a i) v)))

(defun %snc-array (type n)
  (let ((a (make-array n :element-type type)))
    (dotimes (i n a) (setf (aref a i) (coerce i type)))))

(defun %snc-pack (kind values)
  (apply (symbol-function (intern (format nil "MAKE-~A" kind) "DOTCL-SIMD"))
         values))

(defun %snc-lanes (pack)
  (multiple-value-list
   (funcall (intern (format nil "~A-VALUES" (string (type-of pack))) "DOTCL-SIMD")
            pack)))

(deftest-emitting-only simd-no-check-access.read-values
  (loop for (type kind) in *snc-kinds*
        collect (let ((read (compile nil (%snc-read-form type kind))))
                  (%snc-lanes (funcall read (%snc-array type 32) 0))))
  ((0.0d0 1.0d0)
   (0.0 1.0 2.0 3.0)
   (0 1)
   (0 1)
   (0 1 2 3)
   (0 1 2 3 4 5 6 7)
   (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))

(deftest-emitting-only simd-no-check-access.write-values
  (loop for (type kind) in *snc-kinds*
        for n = (%snc-lane-count kind)
        collect (let* ((a (%snc-array type 32))
                       (values (loop for j below n collect (coerce (+ 10 j) type)))
                       (write (compile nil (%snc-write-form type kind))))
                  (funcall write a 1 (%snc-pack kind values))
                  (loop for j below n collect (aref a (1+ j)))))
  ((10.0d0 11.0d0)
   (10.0 11.0 12.0 13.0)
   (10 11)
   (10 11)
   (10 11 12 13)
   (10 11 12 13 14 15 16 17)
   (10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))

(deftest-compiled-only simd-no-check-access.shape
  (loop for (type kind) in *snc-kinds*
        append (let ((lane (%snc-lane-name kind))
                     (read-name (%snc-name kind "READ"))
                     (write-name (%snc-name kind "WRITE")))
                 (eval `(defun ,read-name ,@(cdr (%snc-read-form type kind))))
                 (eval `(defun ,write-name ,@(cdr (%snc-write-form type kind))))
                 (let ((read-text (%flat-text (dotcl:function-sil (symbol-function read-name))))
                       (write-text (%flat-text (dotcl:function-sil (symbol-function write-name)))))
                   (list (and (search (format nil "Simd.LoadNoCheck.~A" lane) read-text) t)
                         (and (search (format nil "Simd.StoreNoCheck.~A" lane) write-text) t)))))
  (t t t t t t t t t t t t t t))
