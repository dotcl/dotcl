;;; (FUNCALL F (AREF A I...)) goes through Runtime.FuncallAref<rank>L[L][M], and
;;; (FUNCALL F X) with X an element of a declared integer array through
;;; Runtime.FuncallLong1[L][M]: the element reaches F's typed entry without a
;;; box. The M suffix is the value-mode argument; the variants without it are
;;; what the compiler emits with value modes off, so the second set of
;;; functions is compiled that way. Values and errors agree with SBCL 2.6.8.

(setf dotcl:*save-sil* t)

(declaim (ftype (function ((unsigned-byte 16)) (unsigned-byte 16)) %fm-inc))
(defun %fm-inc (x) (logand 65535 (1+ x)))
(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8)) %fm-u8))
(defun %fm-u8 (x) (logxor x 1))
(defun %fm-list (x) (list x))
(defun %fm-two (x) (values x 99))
(defun %fm-neg (x) (- x))
(defmacro %fm-defs (suffix)
  `(progn
     (defun ,(intern (format nil "%FM-A1-~A" suffix) "COMMON-LISP-USER") (f a i) (declare (fixnum i)) (list (funcall f (aref a i))))
     (defun ,(intern (format nil "%FM-A2-~A" suffix) "COMMON-LISP-USER") (f a i j) (declare (fixnum i j)) (list (funcall f (aref a i j))))
     (defun ,(intern (format nil "%FM-A1L-~A" suffix) "COMMON-LISP-USER") (f a i) (declare (fixnum i))
       (let ((s 0)) (declare (fixnum s)) (incf s (the fixnum (funcall f (aref a i)))) s))
     (defun ,(intern (format nil "%FM-A2L-~A" suffix) "COMMON-LISP-USER") (f a i j) (declare (fixnum i j))
       (let ((s 0)) (declare (fixnum s)) (incf s (the fixnum (funcall f (aref a i j)))) s))
     (defun ,(intern (format nil "%FM-L1-~A" suffix) "COMMON-LISP-USER") (f a i) (declare (fixnum i) (type (simple-array (unsigned-byte 16) (*)) a))
       (list (funcall f (aref a i))))
     (defun ,(intern (format nil "%FM-L1L-~A" suffix) "COMMON-LISP-USER") (f a i) (declare (fixnum i) (type (simple-array (unsigned-byte 16) (*)) a))
       (let ((s 0)) (declare (fixnum s)) (incf s (the fixnum (funcall f (aref a i)))) s))
     (defun ,(intern (format nil "%FM-L2-~A" suffix) "COMMON-LISP-USER") (f a i j) (declare (fixnum i j) (type (simple-array (unsigned-byte 16) (* *)) a))
       (list (funcall f (aref a i j))))))
(defparameter *fm-u16* (make-array 3 :element-type '(unsigned-byte 16) :initial-contents '(0 65535 7)))
(defparameter *fm-2d* (make-array '(2 2) :element-type '(unsigned-byte 16) :initial-contents '((1 2) (3 65535))))
(defparameter *fm-t* (vector 5 -5 "s"))
(defparameter *fm-s32* (make-array 2 :element-type '(signed-byte 32) :initial-contents '(-2147483648 2147483647)))
(defun %fm-te (thunk) (handler-case (funcall thunk) (type-error () :type-error) (error () :error)))
(defun %fm-run (sfx)
  (flet ((fn (n) (symbol-function (find-symbol (format nil "%FM-~A-~A" n sfx) "COMMON-LISP-USER"))))
   (list
   (list (funcall (fn "A1") #'%fm-list *fm-t* 1) (funcall (fn "A1") #'%fm-list *fm-u16* 1) (funcall (fn "A1") #'%fm-list *fm-s32* 0)
         (funcall (fn "A1") #'%fm-two *fm-u16* 2) (funcall (fn "A1") 'identity *fm-u16* 0)
         (%fm-te (lambda () (funcall (fn "A1") #'%fm-list *fm-u16* 3))) (%fm-te (lambda () (funcall (fn "A1") #'%fm-inc *fm-t* 2))))
   (list (funcall (fn "A2") #'%fm-list *fm-2d* 1 1) (funcall (fn "A2") #'%fm-list *fm-2d* 0 0) (funcall (fn "A2") #'%fm-neg *fm-2d* 1 0)
         (%fm-te (lambda () (funcall (fn "A2") #'%fm-list *fm-2d* 2 0))) (%fm-te (lambda () (funcall (fn "A2") #'%fm-u8 *fm-2d* 1 1))))
   (list (funcall (fn "A1L") #'%fm-neg *fm-u16* 1) (funcall (fn "A1L") #'identity *fm-u16* 2) (funcall (fn "A1L") #'%fm-inc *fm-u16* 1)
         (%fm-te (lambda () (funcall (fn "A1L") #'%fm-u8 *fm-u16* 1))) (%fm-te (lambda () (funcall (fn "A1L") #'identity *fm-t* 2))))
   (list (funcall (fn "A2L") #'%fm-neg *fm-2d* 1 1) (funcall (fn "A2L") #'identity *fm-2d* 0 1) (%fm-te (lambda () (funcall (fn "A2L") #'%fm-u8 *fm-2d* 1 1))))
   (list (funcall (fn "L1") #'%fm-list *fm-u16* 1) (funcall (fn "L1") #'%fm-inc *fm-u16* 1) (funcall (fn "L1L") #'%fm-neg *fm-u16* 1)
         (funcall (fn "L1L") #'%fm-inc *fm-u16* 2) (%fm-te (lambda () (funcall (fn "L1L") #'%fm-u8 *fm-u16* 1))) (%fm-te (lambda () (funcall (fn "L1") #'%fm-list *fm-u16* 3)))
         (%fm-te (lambda () (funcall (fn "L1") #'%fm-list *fm-s32* 0))))
   (list (funcall (fn "L2") #'%fm-list *fm-2d* 1 1) (funcall (fn "L2") #'%fm-inc *fm-2d* 1 1) (%fm-te (lambda () (funcall (fn "L2") #'%fm-u8 *fm-2d* 1 1)))))))

(%fm-defs "ON")

(defun %fm-build-without-modes ()
  (let ((old dotcl-internal::*mv-mode-elision*))
    (setq dotcl-internal::*mv-mode-elision* nil)
    (unwind-protect (eval '(%fm-defs "OFF"))
      (setq dotcl-internal::*mv-mode-elision* old))))
(%fm-build-without-modes)

(deftest funcall-aref-modes.values-with-modes
  (%fm-run "ON")
  ((((-5)) ((65535)) ((-2147483648)) (7) (0) :type-error :type-error)
      (((65535)) ((1)) (-3) :type-error :type-error)
      (-65535 7 0 :type-error :type-error) (-65535 2 :type-error)
      (((65535)) (0) -65535 8 :type-error :type-error :type-error)
      (((65535)) (0) :type-error)))

(deftest funcall-aref-modes.values-without-modes
  (%fm-run "OFF")
  ((((-5)) ((65535)) ((-2147483648)) (7) (0) :type-error :type-error)
      (((65535)) ((1)) (-3) :type-error :type-error)
      (-65535 7 0 :type-error :type-error) (-65535 2 :type-error)
      (((65535)) (0) -65535 8 :type-error :type-error :type-error)
      (((65535)) (0) :type-error)))

(defun %fm-has (needle fn)
  (and (search needle (%flat-text (dotcl:function-sil fn))) t))
(defun %fm-names (fn)
  (let* ((s (%flat-text (dotcl:function-sil fn))) (r '()) (p 0))
    (loop (let ((q (search "Runtime.Funcall" s :start2 p)))
            (unless q (return))
            (let ((e (position-if (lambda (c) (member c '(#\Space #\" #\)))) s :start q)))
              (pushnew (subseq s q e) r :test #'string=)
              (setq p (1+ q)))))
    (sort r #'string<)))

(deftest-compiled-only funcall-aref-modes.shape
  (list (%fm-names #'%fm-a1-on) (%fm-names #'%fm-a2-on)
        (%fm-names #'%fm-a1l-on) (%fm-names #'%fm-a2l-on)
        (%fm-names #'%fm-l1-on) (%fm-names #'%fm-l1l-on) (%fm-names #'%fm-l2-on)
        (%fm-names #'%fm-a1-off) (%fm-names #'%fm-a2-off)
        (%fm-names #'%fm-a1l-off) (%fm-names #'%fm-a2l-off)
        (%fm-names #'%fm-l1-off) (%fm-names #'%fm-l1l-off) (%fm-names #'%fm-l2-off))
  (("Runtime.FuncallAref1LM") ("Runtime.FuncallAref2LM")
   ("Runtime.FuncallAref1LL") ("Runtime.FuncallAref2LL")
   ("Runtime.FuncallLong1M") ("Runtime.FuncallLong1L") ("Runtime.FuncallLong1M")
   ("Runtime.FuncallAref1L") ("Runtime.FuncallAref2L")
   ("Runtime.FuncallAref1LL") ("Runtime.FuncallAref2LL")
   ("Runtime.FuncallLong1") ("Runtime.FuncallLong1L") ("Runtime.FuncallLong1")))

;;; A long value is consumed as one value, so its calls carry no mode.
(deftest-compiled-only funcall-aref-modes.long-no-mode
  (list (member "Runtime.FuncallAref1LLM" (%fm-names #'%fm-a1l-on))
        (member "Runtime.FuncallAref2LLM" (%fm-names #'%fm-a2l-on))
        (member "Runtime.FuncallLong1LM" (%fm-names #'%fm-l1l-on))
        (member "Runtime.FuncallAref1LLM" (%fm-names #'%fm-a1l-off))
        (member "Runtime.FuncallAref2LLM" (%fm-names #'%fm-a2l-off))
        (member "Runtime.FuncallLong1LM" (%fm-names #'%fm-l1l-off)))
  (nil nil nil nil nil nil))
