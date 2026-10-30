;;; An IF whose arms are fixnum products or left shifts, cut by LOGAND / DPB
;;; to a width: the IF was taken as fixnum (the mask allows the arms to wrap),
;;; but the arms were then computed as integers and unboxed, so a bignum arm
;;; signalled "Unable to cast object of type 'DotCL.Bignum' to type
;;; 'DotCL.Fixnum'". Expected values are SBCL's.

(defmacro %ialm (name expected form &rest args)
  `(deftest-emitting-only ,(intern (format nil "IF-ARM-UNDER-LOGAND-MASK.~a" name))
     (let ((*error-output* (make-broadcast-stream)))
       (funcall (compile nil ',form) ,@args))
     ,expected))

(%ialm "MUL" 164
  (lambda (c p) (declare (fixnum c)) (logand (if p (* c c) 5) 255))
  13917432584742 t)

(%ialm "MUL-ELSE" 5
  (lambda (c p) (declare (fixnum c)) (logand (if p (* c c) 5) 255))
  13917432584742 nil)

(%ialm "ASH" 0
  (lambda (c p) (declare (fixnum c)) (logand (if p (ash c 46) 5) 255))
  13917432584742 t)

(%ialm "DPB" 0
  (lambda (c) (declare (type (integer 0 20606875579917) c))
    (dpb (if nil 5 (ash c 46)) (byte 18 2) 0))
  13917432584742)

(%ialm "SAFETY-0" 164
  (lambda (c p) (declare (optimize (safety 0)) (fixnum c)) (logand (if p (* c c) 5) 255))
  13917432584742 t)

(%ialm "RANDOM-FORM" 10332688980246235556213686272000000
  (lambda (a b c d)
    (declare (type (integer -555573498 442012280899) a)
             (type (integer -27 35) b)
             (type (integer 12854142 20606875579917) c)
             (type (integer 204962404526648387 496916055545883989) d))
    (declare (ignorable a b c d))
    (declare (optimize (safety 0) (compilation-speed 0) (speed 0) (debug 3) (space 3)))
    (ash (/ (restart-bind nil (multiple-value-prog1 -133546875 4611686018427387911 b)) -1)
         (min 86 (coerce (dpb (if nil (if (equal a b) d -1) (ash c (min 46 134217728)))
                              (byte 18 2) 2199023255552)
                         'integer))))
  402954438308 28 13917432584742 291615679326163514)
