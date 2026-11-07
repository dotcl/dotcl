;;; (READ-BYTE stream [eof-error-p [eof-value]]) with one to three arguments is a direct
;;; call with no argument array. The values, end-of-file handling and errors are as for
;;; the function called through APPLY.

(defun %rbd-path () (regression-temp-file (format nil "rbd-~a" (random 1000000))))

(defmacro %rbd-with-bytes ((var bytes &rest open-args) &body body)
  `(let ((path (%rbd-path)))
     (with-open-file (o path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
       (dolist (b ,bytes) (write-byte b o)))
     (unwind-protect (with-open-file (,var path :element-type ,(or (getf open-args :element-type) ''(unsigned-byte 8)))
                       ,@body)
       (ignore-errors (delete-file path)))))

(deftest read-byte-direct.one-arg-and-eof
  (%rbd-with-bytes (s '(1 2 255))
    (list (read-byte s) (read-byte s) (read-byte s)
          (handler-case (read-byte s) (end-of-file () :eof-error))))
  (1 2 255 :eof-error))

(deftest read-byte-direct.eof-arguments
  (%rbd-with-bytes (s '(7))
    (list (read-byte s nil) (read-byte s nil) (read-byte s nil :end) (read-byte s nil nil)
          (handler-case (read-byte s t :never) (end-of-file () :eof-error))))
  (7 nil :end nil :eof-error))

(deftest read-byte-direct.same-as-apply
  (%rbd-with-bytes (s '(10 20 30 40))
    (list (read-byte s) (apply #'read-byte (list s)) (read-byte s nil) (apply #'read-byte (list s nil :x))))
  (10 20 30 40))

(deftest read-byte-direct.wider-and-signed-elements
  (list (%rbd-with-bytes (s '(1 0 2 0) :element-type '(unsigned-byte 16)) (list (read-byte s) (read-byte s)))
        (%rbd-with-bytes (s '(255 1) :element-type '(signed-byte 8)) (list (read-byte s) (read-byte s))))
  ((1 2) (-1 1)))

(deftest read-byte-direct.errors
  (let ((path (%rbd-path)))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string "abc" o))
    (unwind-protect
         (list (with-open-file (s path) (handler-case (read-byte s) (error () :error)))
               (handler-case (read-byte (make-string-input-stream "x")) (error () :error))
               (handler-case (read-byte (make-string-output-stream)) (error () :error))
               (handler-case (read-byte 5) (error () :error))
               (let ((s (open path :element-type '(unsigned-byte 8)))) (close s) (handler-case (read-byte s) (error () :error))))
      (ignore-errors (delete-file path))))
  (:error :error :error :error :error))

(deftest read-byte-direct.too-many-arguments
  (%rbd-with-bytes (s '(1))
    (handler-case (eval `(read-byte ,s t nil :extra)) (error () :error)))
  :error)

(deftest read-byte-direct.binary-after-read-sequence
  (%rbd-with-bytes (s '(1 2 3 4 5))
    (let ((v (make-array 2 :element-type '(unsigned-byte 8))))
      (list (read-sequence v s) (coerce v 'list) (read-byte s) (file-position s))))
  (2 (1 2) 3 3))
