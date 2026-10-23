;;; A function with a typed entry that records a frame also has a framed typed
;;; entry: it links the frame itself (raw arguments) around the typed body, and
;;; a caller that knows the FTYPE enters it by its address. The frame list must
;;; be right after every way out of the callee, deep recursion through it must
;;; end in a STORAGE-CONDITION, and an entry built in memory must stay callable
;;; after a collection.

(defun %tfe-depth () (length (dotcl:backtrace)))

(declaim (notinline %tfe-add %tfe-quiet %tfe-thrower %tfe-ping %tfe-pong %tfe-u8))
(declaim (ftype (function (fixnum fixnum) fixnum) %tfe-add))
(defun %tfe-add (a b) (+ a b))
(declaim (ftype (function (fixnum) fixnum) %tfe-quiet))
(defun %tfe-quiet (n) (declare (optimize (debug 0))) (1+ n))
(declaim (ftype (function (fixnum) fixnum) %tfe-thrower))
(defun %tfe-thrower (n)
  (cond ((= n 0) (throw :tfe n))
        ((= n 1) (error "tfe ~a" n))
        (t n)))
(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8)) %tfe-u8))
(defun %tfe-u8 (x) (logand (1+ x) 255))

(deftest-compiled-only typed-framed-entry.state
  (list (dotnet:static "DotCL.LispFunction" "TypedEntryState" #'%tfe-add)
        (dotnet:static "DotCL.LispFunction" "TypedEntryState" #'%tfe-quiet))
  ("framed" "typed"))

(defun %tfe-call-thrower (n) (list (%tfe-thrower n)))
(defun %tfe-call-u8 (x) (declare (fixnum x)) (list (%tfe-u8 x)))

(deftest typed-framed-entry.frame-list-after-exits
  (let ((base (%tfe-depth)))
    (list (%tfe-add 1 2)
          (%tfe-quiet 1)
          (- (%tfe-depth) base)
          (catch :tfe (%tfe-call-thrower 0))
          (- (%tfe-depth) base)
          (handler-case (%tfe-call-thrower 1) (simple-error () :error))
          (- (%tfe-depth) base)
          (block out (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from out :escaped))))
                       (%tfe-call-thrower 1)))
          (- (%tfe-depth) base)
          (handler-case (%tfe-call-u8 300) (type-error () :type-error))
          (- (%tfe-depth) base)
          (%tfe-call-thrower 5)))
  (3 2 0 0 0 :error 0 :escaped 0 :type-error 0 (5)))

;;; Non-tail mutual recursion through framed typed entries: the frame's depth
;;; check turns a runaway recursion into a STORAGE-CONDITION.
(declaim (ftype (function (fixnum) fixnum) %tfe-ping %tfe-pong))
(defun %tfe-ping (n) (if (= n 0) 0 (1+ (%tfe-pong (1- n)))))
(defun %tfe-pong (n) (if (= n 0) 0 (1+ (%tfe-ping (1- n)))))

(deftest typed-framed-entry.deep-recursion
  (call-on-small-stack
   (lambda ()
     (let ((base (%tfe-depth)))
       (list (%tfe-ping 1000)
             (handler-case (%tfe-ping 100000000) (storage-condition () :overflow))
             (- (%tfe-depth) base)))))
  (1000 :overflow 0))

;;; Defined in memory (EVAL), called after collections.
(deftest typed-framed-entry.in-memory-survives-gc
  (progn
    (eval '(progn
            (declaim (notinline %tfe-mem))
            (declaim (ftype (function (double-float fixnum) double-float) %tfe-mem))
            (defun %tfe-mem (x n) (* x n))
            (defun %tfe-mem-caller (k)
              (let ((s 0d0))
                (declare (double-float s))
                (dotimes (i k s) (setq s (+ s (%tfe-mem 0.5d0 i))))))))
    (let ((r '()))
      (dotimes (i 3)
        (dotnet:static "System.GC" "Collect")
        (dotnet:static "System.GC" "WaitForPendingFinalizers")
        (push (funcall '%tfe-mem-caller 100) r))
      r))
  (2475d0 2475d0 2475d0))
