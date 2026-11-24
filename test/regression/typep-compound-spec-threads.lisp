;;; The first TYPEP against a type defined by DEFTYPE remembers that its compound
;;; expansion was validated. Two threads meeting the same new type at once both
;;; missed the lookup and both added it, and the second died with "An item with the
;;; same key has already been added" (seen as babel:string-to-octets failing in some
;;; of 50 client threads).

(require "dotcl-thread")

(defun %tcst-run (nthreads ntypes)
  (let* ((names (loop for i below ntypes
                      collect (let ((name (intern (format nil "TCST-TYPE-~D" i) :cl-user)))
                                (eval `(deftype ,name () '(simple-array (unsigned-byte 8) (,(+ 1 i)))))
                                name)))
         (names (coerce names 'vector))
         (errors 0)
         (released -1)
         (lock (dotcl-thread:make-lock))
         (threads (loop repeat nthreads
                        collect (dotcl-thread:make-thread
                                 (lambda ()
                                   (dotimes (i ntypes)
                                     (loop until (>= released i) do (dotcl-thread:thread-yield))
                                     (handler-case (coerce (make-array (+ 1 i) :element-type '(unsigned-byte 8)) (aref names i))
                                       (error ()
                                         (dotcl-thread:with-lock-held (lock) (incf errors))))))))))
    (dotimes (i ntypes)
      (setf released i)
      (sleep 0.0005))
    (dolist (th threads) (dotcl-thread:thread-join th))
    errors))

(deftest typep-compound-spec-threads.no-duplicate-key-error
  (%tcst-run 8 600)
  0)
