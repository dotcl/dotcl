;;; A function with a typed entry (an FTYPE giving every argument and the result
;;; a raw numeric type) is called by an untyped caller through its boxed shell.
;;; The shell links the function's own debugger frame, as the body of an
;;; ordinary function does, so a FUNCALL of it goes through the entry pointer.
;;; The frame must be there while the body runs, whatever the call path, and
;;; gone after every way out of it.

(defvar *tss-bt* nil)
(defun %tss-probe () (setf *tss-bt* (dotcl:backtrace)) 0)
(defun %tss-depth () (length (dotcl:backtrace)))

(declaim (ftype (function (fixnum) (values fixnum &optional)) %tss-add1))
(defun %tss-add1 (n)
  (declare (fixnum n))
  (the fixnum (+ n (the fixnum (values (%tss-probe))) 1)))

(declaim (ftype (function (fixnum fixnum) (values fixnum &optional)) %tss-sub))
(defun %tss-sub (a b) (declare (fixnum a b)) (the fixnum (- a b)))

(declaim (ftype (function (fixnum) (values fixnum &optional)) %tss-quiet))
(defun %tss-quiet (n)
  (declare (fixnum n) (optimize (debug 0)))
  (the fixnum (+ n (the fixnum (values (%tss-probe))))))

(defun %tss-in-bt-p (name) (and (member name *tss-bt* :test #'string=) t))

(deftest-compiled-only typed-shell-self-framed.entry
  (list (dotnet:static "DotCL.LispFunction" "EntryState" #'%tss-add1)
        (dotnet:static "DotCL.LispFunction" "EntryState" #'%tss-sub)
        (dotnet:static "DotCL.LispFunction" "EntryState" #'%tss-quiet))
  ("entry" "entry" "none"))

(deftest-compiled-only typed-shell-self-framed.frame-on-every-path
  (let ((f #'%tss-add1) (r '()))
    (setf *tss-bt* nil) (push (list (funcall f 1) (%tss-in-bt-p "%TSS-ADD1")) r)
    (setf *tss-bt* nil) (push (list (apply f '(2)) (%tss-in-bt-p "%TSS-ADD1")) r)
    (setf *tss-bt* nil) (push (list (funcall '%tss-add1 3) (%tss-in-bt-p "%TSS-ADD1")) r)
    (setf *tss-bt* nil) (push (list (car (mapcar f '(4))) (%tss-in-bt-p "%TSS-ADD1")) r)
    (setf *tss-bt* nil) (push (list (funcall #'%tss-quiet 5) (%tss-in-bt-p "%TSS-QUIET")) r)
    (nreverse r))
  ((2 t) (3 t) (4 t) (5 t) (5 nil)))

(deftest-compiled-only typed-shell-self-framed.frame-list-after-exits
  (let* ((base (%tss-depth))
         (f #'%tss-add1)
         (g #'%tss-sub))
    (list (funcall f 10)
          (funcall g 10 3)
          (- (%tss-depth) base)
          (handler-case (funcall f "x") (type-error () :type-error))
          (- (%tss-depth) base)
          (handler-case (funcall g 1 1.5) (type-error () :type-error))
          (handler-case (apply g '(1 2 3)) (program-error () :program-error))
          (- (%tss-depth) base)))
  (11 7 0 :type-error 0 :type-error :program-error 0))
