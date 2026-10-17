;;; An argument of an INLINE function computed boxed (a call, a variable that
;;; is not a word) and declared (unsigned-byte 64) is checked once where it is
;;; computed, and signals a TYPE-ERROR when it is not one. The expression has no
;;; generic copy for that case (ironclad's MOD64+ of SHA-512's local function
;;; results: four such copies a round).

(setf dotcl:*save-sil* t)

(defun %uipc-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(declaim (inline %uipc-add %uipc-shr))
(defun %uipc-add (a b)
  (declare (type (unsigned-byte 64) a b))
  (ldb (byte 64 0) (+ a b)))
;; A right shift reads the high bits: the exact value is needed.
(defun %uipc-shr (a b)
  (declare (type (unsigned-byte 64) a b))
  (logxor (ash a -7) b))

(declaim (notinline %uipc-boxed))
(defun %uipc-boxed (x) x)

(defun %uipc-sum (w x)
  (declare (type (simple-array (unsigned-byte 64) (*)) w))
  (setf (aref w 1) (%uipc-add (aref w 0) (%uipc-add (%uipc-boxed x) (aref w 0))))
  w)

(defun %uipc-exact (w x)
  (declare (type (simple-array (unsigned-byte 64) (*)) w))
  (setf (aref w 1) (%uipc-shr (%uipc-boxed x) (aref w 0)))
  w)

(deftest-compiled-only uipc-no-generic-copy
  (list (%uipc-sil-count #'%uipc-sum "IsU64")
        (%uipc-sil-count #'%uipc-sum "Runtime.Add")
        (plusp (%uipc-sil-count #'%uipc-sum "CheckU64Arg"))
        (%uipc-sil-count #'%uipc-exact "IsU64")
        (plusp (%uipc-sil-count #'%uipc-exact "CheckU64Arg")))
  (0 0 t 0 t))

;; The emit-free interpreter does not check declared parameter types.
(deftest-emitting-only uipc-values
  (flet ((run (fn x)
           (let ((w (make-array 2 :element-type '(unsigned-byte 64) :initial-element 0)))
             (setf (aref w 0) 2)
             (handler-case (aref (funcall fn w x) 1)
               (type-error () :type-error)))))
    (list (run #'%uipc-sum #xFFFFFFFFFFFFFFFF)
          (run #'%uipc-sum 5)
          (run #'%uipc-sum -1)
          (run #'%uipc-sum (+ (expt 2 64) 7))
          (run #'%uipc-sum :x)
          (run #'%uipc-sum 1.5d0)
          (run #'%uipc-exact #x8000000000000000)
          (run #'%uipc-exact (expt 2 70))))
  (3 9 :type-error :type-error :type-error :type-error #x0100000000000002 :type-error))
