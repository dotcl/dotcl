;;; The body of an ordinary function records its own debugger frame, with no
;;; try/finally: it unlinks the frame before it returns or makes a tail call. A
;;; non-local exit leaves the frame list pointing into the native frames it
;;; unwound, so whatever catches the exit puts the list back first. These tests
;;; check the list after every kind of exit, from compiled handlers and from C#
;;; (the tree-walk evaluator's), across threads, through redefinition, and
;;; after a stack overflow.

(require "dotcl-thread")

(defun %sf-depth () (length (dotcl:backtrace)))
(defvar *sf-sink* nil)

(defun %sf-throw-at (n kind)
  (if (= n 0)
      (ecase kind
        (:error (error "self-framed ~A" n))
        (:throw (throw '%sf-tag :thrown))
        (:signal (signal 'simple-condition :format-control "sf")))
      (prog1 (%sf-throw-at (1- n) kind) (setq *sf-sink* n))))

(defun %sf-after-handler-case (n)
  (handler-case (%sf-throw-at n :error)
    (error () (values (%sf-depth)))))

(defun %sf-after-catch (n)
  (catch '%sf-tag (%sf-throw-at n :throw))
  (values (%sf-depth)))

(defun %sf-after-return-from (n)
  (block out
    (%sf-walk n (lambda () (return-from out nil))))
  (values (%sf-depth)))

(defun %sf-walk (n k) (if (= n 0) (funcall k) (prog1 (%sf-walk (1- n) k) (setq *sf-sink* n))))

(defun %sf-after-go (n)
  (let ((i 0))
    (tagbody
     again
       (incf i)
       (when (< i 3) (%sf-walk n (lambda () (go again)))))
    (values (%sf-depth))))

(defun %sf-after-restart (n)
  (restart-case (%sf-walk n (lambda () (invoke-restart 'sf-out)))
    (sf-out () nil))
  (values (%sf-depth)))

(defvar *sf-cleanup-depth* nil)
(defun %sf-cleanup-depth (n)
  (catch '%sf-tag
    (unwind-protect (%sf-throw-at n :throw)
      (setf *sf-cleanup-depth* (%sf-depth))))
  *sf-cleanup-depth*)

(defun %sf-handler-bind-depth (n)
  (let ((seen nil))
    (handler-bind ((simple-condition (lambda (c) (declare (ignore c))
                                       (setf seen (%sf-depth)))))
      (%sf-throw-at n :signal))
    seen))

;; Each function adds one frame, and is entered from the test with one.
(deftest self-framed.frame-list-after-exits
  (let ((base (%sf-depth)))
    (list (- (%sf-after-handler-case 30) base)
          (- (%sf-after-catch 30) base)
          (- (%sf-after-return-from 30) base)
          (- (%sf-after-go 30) base)
          (- (%sf-after-restart 30) base)
          (- (%sf-cleanup-depth 30) base)
          (- (%sf-depth) base)))
  (1 1 1 1 1 1 0))

;; A handler run by SIGNAL runs on top of the frames that signalled.
(deftest-emitting-only self-framed.handler-bind-sees-the-signalling-frames
  (- (%sf-handler-bind-depth 30) (%sf-depth))
  ;; %SF-HANDLER-BIND-DEPTH and 31 %SF-THROW-AT
  32)

;; The tree-walk evaluator's HANDLER-CASE and CATCH are C# catch handlers.
(deftest self-framed.frame-list-after-an-interpreted-handler
  (let ((base (%sf-depth)))
    (let ((dotcl:*evaluator-mode* :interpret))
      (list (- (eval '(handler-case (%sf-throw-at 30 :error) (error () (%sf-depth)))) base)
            (- (eval '(progn (catch '%sf-tag (%sf-throw-at 30 :throw)) (%sf-depth))) base)
            (- (%sf-depth) base))))
  (0 0 0))

;; Entered from the runtime's sequence functions (written in C#), at every arity.
(defvar *sf-seen* nil)
(defun %sf-pred0 () (push (%sf-depth) *sf-seen*) nil)
(defun %sf-pred1 (x) (push (%sf-depth) *sf-seen*) (when (eql x 3) (%sf-throw-at 20 :error)) nil)
(defun %sf-pred2 (x y) (push (%sf-depth) *sf-seen*) (when (eql x 3) (%sf-throw-at 20 :throw)) (< x y))
(defun %sf-pred3 (x y z) (push (%sf-depth) *sf-seen*) (list x y z))
(defun %sf-pred4 (x y z w) (push (%sf-depth) *sf-seen*) (list x y z w))

(deftest-compiled-only self-framed.entered-from-the-runtime
  (let ((base (%sf-depth)))
    (setq *sf-seen* nil)
    (list (find-if #'%sf-pred1 '(1 2))
          (sort (list 2 1) #'%sf-pred2)
          (funcall #'%sf-pred0)
          (apply #'%sf-pred3 '(1 2 3))
          (apply #'%sf-pred4 '(1 2 3 4))
          (handler-case (find-if #'%sf-pred1 '(1 2 3)) (error () (- (%sf-depth) base)))
          (catch '%sf-tag (mapcar #'%sf-pred2 '(1 3) '(2 4)))
          (- (%sf-depth) base)
          (let ((dotcl:*evaluator-mode* :interpret))
            (eval '(handler-case (find-if #'%sf-pred1 '(3)) (error () :caught))))
          (- (%sf-depth) base)
          (remove-duplicates (mapcar (lambda (d) (- d base)) *sf-seen*))))
  (nil (1 2) nil (1 2 3) (1 2 3 4) 0 :thrown 0 :caught 0 (1)))

;;; --- redefinition --------------------------------------------------------

(defun %sf-callee (x) (list :one x))
(defun %sf-caller (x) (values (%sf-callee x)))
(defun %sf-tail-caller (x) (%sf-callee x))

(deftest self-framed.redefinition-is-seen-by-callers
  (let ((r1 (list (%sf-caller 1) (%sf-tail-caller 1))))
    (eval '(defun %sf-callee (x) (list :two x)))
    (let ((r2 (list (%sf-caller 2) (%sf-tail-caller 2))))
      (setf (fdefinition '%sf-callee) (let ((k :three)) (lambda (x) (list k x))))
      (let ((r3 (list (%sf-caller 3) (%sf-tail-caller 3))))
        (eval '(defun %sf-callee (x &optional (y :four)) (list y x)))
        (list r1 r2 r3 (list (%sf-caller 4) (%sf-tail-caller 4))))))
  (((:one 1) (:one 1)) ((:two 2) (:two 2)) ((:three 3) (:three 3)) ((:four 4) (:four 4))))

(defun %sf-redefine-self (n)
  (when (= n 5) (eval '(defun %sf-redefine-self (n) (list :new n))))
  (if (= n 0) :old (car (list (funcall (symbol-function '%sf-redefine-self) (1- n))))))

(deftest self-framed.redefined-while-running
  (list (%sf-redefine-self 8) (%sf-redefine-self 8))
  ((:new 4) (:new 8)))

;;; --- threads ---------------------------------------------------------------

(deftest self-framed.threads-keep-their-own-frames
  (let* ((threads
           (loop for k below 4
                 collect (let ((k k))
                           (dotcl-thread:make-thread
                            (lambda ()
                              (let ((base (%sf-depth)) (bad 0))
                                (dotimes (i 200)
                                  (unless (and (= (- (%sf-after-handler-case (+ k 10)) base) 1)
                                               (= (- (%sf-after-catch (+ k 20)) base) 1)
                                               (= (%sf-depth) base))
                                    (incf bad)))
                                bad)))))))
    (mapcar #'dotcl-thread:thread-join threads))
  (0 0 0 0))

;;; --- (debug 3) -------------------------------------------------------------

(defun %sf-debug-leaf (a)
  (declare (optimize (debug 3)))
  (let ((b (* a 2)))
    (when (= a 0) (error "leaf"))
    (values (%sf-debug-leaf (1- a)) b)))

(defun %sf-debug-catch (n)
  (declare (optimize (debug 3)))
  (let ((mine (+ n 100)))
    (handler-case (%sf-debug-leaf n) (error () nil))
    (list mine (values (%sf-depth)))))

(deftest self-framed.debug-3-after-a-caught-error
  (let ((base (%sf-depth)))
    (destructuring-bind (mine depth) (%sf-debug-catch 20)
      (list mine (- depth base))))
  (120 1))

;;; --- stack overflow --------------------------------------------------------

(defun %sf-no-bottom (n) (1+ (%sf-no-bottom (1+ n))))

(deftest-emitting-only self-framed.stack-overflow-is-a-condition
  (call-on-small-stack
   (lambda ()
     (let ((base (%sf-depth)))
       (list (handler-case (%sf-no-bottom 0) (storage-condition () :overflow))
             (- (%sf-depth) base)
             (- (%sf-after-handler-case 30) base)))))
  (:overflow 0 1))

;;; --- a borrowed frame --------------------------------------------------------

;; A named function tail-calls a closure, which tail-calls a named function:
;; the last one takes over the first one's frame.
(defun %sf-borrow-end (x) (declare (ignore x)) (dotcl:backtrace))
(defun %sf-borrow-start (x) (funcall (lambda (y) (%sf-borrow-end y)) x))
(defun %sf-borrow-throws (x) (declare (ignore x)) (error "borrowed"))
(defun %sf-borrow-start-throw (x) (funcall (lambda (y) (%sf-borrow-throws y)) x))

(deftest-emitting-only self-framed.borrowed-frame
  (let ((base (%sf-depth)))
    (list (subseq (%sf-borrow-start 1) 0 1)
          (- (length (%sf-borrow-start 1)) base)
          (handler-case (%sf-borrow-start-throw 1) (error () :caught))
          (- (%sf-depth) base)))
  (("%SF-BORROW-END") 0 :caught 0))

;;; --- C# that catches and carries on ------------------------------------------

;; A callback's error is contained by the delegate boundary, which returns to
;; List.ForEach and gets called again. The callee with five arguments is framed
;; by InvokeN in a try/finally; that finally must not put the callback's frame,
;; which is gone by then, back on the list.
(defvar *sf-cb-log* nil)
(defun %sf-cb-inner (a b c d e)
  (declare (ignore b c d e))
  (case a
    (1 (error "contained"))
    (3 (throw '%sf-tag :thrown)))
  (push (%sf-depth) *sf-cb-log*)
  a)
(defun %sf-cb-outer (x) (car (list (%sf-cb-inner x 1 2 3 4))))

(defun %sf-cb-cleanup (x) (setq *sf-sink* x))
(defun %sf-cb-protected (x)
  (unwind-protect
       (progn (when (eql x 1) (%sf-throw-at 5 :error))
              (when (eql x 3) (throw '%sf-tag :thrown))
              (push (%sf-depth) *sf-cb-log*))
    (%sf-cb-cleanup x)))

(defun %sf-for-each (fn &rest xs)
  (let ((l (dotnet:new "System.Collections.Generic.List`1[System.Int32]"))
        (*error-output* (make-broadcast-stream)))
    (dolist (x xs) (dotnet:invoke l "Add" x))
    (catch '%sf-tag
      (dotnet:invoke l "ForEach" (dotnet:make-delegate "System.Action`1[System.Int32]" fn)))))

(deftest self-framed.frame-list-after-a-callback-error-is-contained
  (let ((base (%sf-depth)))
    (flet ((run (fn)
             (setq *sf-cb-log* nil)
             (%sf-for-each fn 2)
             (let ((clean *sf-cb-log*))
               (setq *sf-cb-log* nil)
               (list (%sf-for-each fn 1 2 3)
                     (equal *sf-cb-log* clean)
                     (- (%sf-depth) base)))))
      (list (run #'%sf-cb-outer) (run #'%sf-cb-protected))))
  ((:thrown t 0) (:thrown t 0)))
