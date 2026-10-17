;;; An &optional or &key function has a body for each arity it is commonly
;;; called with. Each of those bodies links the function's own debugger frame, as
;;; the body of a function with required parameters only does, and a compiled
;;; caller enters one of them through the function's entry pointer. Defaults,
;;; supplied-p variables and keyword parsing must not change, the frame must be
;;; there with the arguments as passed, and gone after every way out.

(defun %oks-depth () (length (dotcl:backtrace)))
(defvar *oks-bt* nil)
(defvar *oks-sink* nil)
(defvar *oks-order* nil)

(defun %oks-note (x) (push x *oks-order*) x)

(defun %oks-opt (a &optional (b (%oks-note (list :b a))) (c (%oks-note (list :c a b))))
  (list a b c))
(defun %oks-opt-sp (a &optional (b 2 b-p)) (list a b b-p))
(defun %oks-key (a &key (b (%oks-note (list :b a))) (c 3 c-p))
  (list a b c c-p))
(defun %oks-key-aok (a &key b &allow-other-keys) (list a b))
(defun %oks-opt-mv (a &optional (b 1)) (values a b (+ a b)))
(defun %oks-quiet (a &optional (b 1))
  (declare (optimize (debug 0)))
  (+ a b))

(deftest-compiled-only optional-key-self-framed.entry
  (list (dotnet:static "DotCL.LispFunction" "EntryState" #'%oks-opt)
        (dotnet:static "DotCL.LispFunction" "EntryState" #'%oks-key)
        (dotnet:static "DotCL.LispFunction" "EntryState" #'%oks-quiet))
  ("entry" "entry" "none"))

(deftest optional-key-self-framed.defaults
  (progn
    (setq *oks-order* nil)
    (list (%oks-opt 1) (%oks-opt 1 2) (%oks-opt 1 2 3)
          (funcall #'%oks-opt 4) (apply #'%oks-opt 5 '(6))
          (reverse *oks-order*)))
  ((1 (:b 1) (:c 1 (:b 1))) (1 2 (:c 1 2)) (1 2 3)
   (4 (:b 4) (:c 4 (:b 4))) (5 6 (:c 5 6))
   ((:b 1) (:c 1 (:b 1)) (:c 1 2) (:b 4) (:c 4 (:b 4)) (:c 5 6))))

(deftest optional-key-self-framed.supplied-p
  (list (%oks-opt-sp 1) (%oks-opt-sp 1 nil) (funcall #'%oks-opt-sp 1 5)
        (%oks-key 1) (%oks-key 1 :c nil) (%oks-key 1 :c 4 :b 2))
  ((1 2 nil) (1 nil t) (1 5 t)
   (1 (:b 1) 3 nil) (1 (:b 1) nil t) (1 2 4 t)))

(deftest optional-key-self-framed.keyword-parsing
  (list (%oks-key 1 :b 2 :b 3)
        (%oks-key 1 :c 5 :c 6 :b 7)
        (%oks-key 1 :allow-other-keys t :zz 1)
        (%oks-key-aok 1 :zz 2 :b 3)
        (funcall #'%oks-key 1 :b 9)
        (apply #'%oks-key 1 '(:allow-other-keys nil :b 4))
        (handler-case (funcall #'%oks-key 1 :zz 1) (program-error () :unknown))
        (handler-case (apply #'%oks-key 1 '(:b)) (program-error () :odd))
        (handler-case (funcall #'%oks-opt) (program-error () :too-few))
        (handler-case (apply #'%oks-opt '(1 2 3 4)) (program-error () :too-many)))
  ((1 2 3 nil) (1 7 5 t) (1 (:b 1) 3 nil) (1 3) (1 9 3 nil) (1 4 3 nil)
   :unknown :odd :too-few :too-many))

(deftest optional-key-self-framed.values
  (list (multiple-value-list (%oks-opt-mv 1))
        (multiple-value-list (%oks-opt-mv 1 2))
        (multiple-value-list (funcall #'%oks-opt-mv 3))
        (nth-value 2 (%oks-opt-mv 2 5)))
  ((1 1 2) (1 2 3) (3 1 4) 7))

;;; --- the frame ------------------------------------------------------------

(defun %oks-probe (a &optional (b 0) c)
  (setq *oks-bt* (dotcl:backtrace-with-args))
  (list a b c))
(defun %oks-kprobe (a &key (b 0))
  (setq *oks-bt* (dotcl:backtrace-with-args))
  (list a b))

(defun %oks-top () (car *oks-bt*))

(deftest-compiled-only optional-key-self-framed.frame-on-every-path
  (let ((r '()))
    (%oks-probe 1) (push (%oks-top) r)
    (%oks-probe 1 2) (push (%oks-top) r)
    (%oks-probe 1 2 3) (push (%oks-top) r)
    (funcall #'%oks-probe 4 5) (push (%oks-top) r)
    (apply #'%oks-probe '(6)) (push (%oks-top) r)
    (mapcar #'%oks-probe '(7) '(8)) (push (%oks-top) r)
    (funcall '%oks-probe 9) (push (%oks-top) r)
    (%oks-kprobe 1) (push (%oks-top) r)
    (%oks-kprobe 1 :b 2) (push (%oks-top) r)
    (funcall #'%oks-kprobe 3 :b 4) (push (%oks-top) r)
    (nreverse r))
  (("%OKS-PROBE" 1) ("%OKS-PROBE" 1 2) ("%OKS-PROBE" 1 2 3) ("%OKS-PROBE" 4 5)
   ("%OKS-PROBE" 6) ("%OKS-PROBE" 7 8) ("%OKS-PROBE" 9)
   ("%OKS-KPROBE" 1) ("%OKS-KPROBE" 1 :b 2) ("%OKS-KPROBE" 3 :b 4)))

(defun %oks-throw-at (n kind &optional (step 1))
  (if (<= n 0)
      (ecase kind
        (:error (error "optional ~A" n))
        (:throw (throw '%oks-tag :thrown)))
      (prog1 (%oks-throw-at (- n step) kind) (setq *oks-sink* n))))

(defun %oks-kthrow-at (n &key (kind :error) (step 1))
  (if (<= n 0)
      (ecase kind
        (:error (error "key ~A" n))
        (:throw (throw '%oks-tag :thrown)))
      (prog1 (%oks-kthrow-at (- n step) :kind kind) (setq *oks-sink* n))))

(defun %oks-walk (n k &optional (step 1))
  (if (<= n 0) (funcall k) (prog1 (%oks-walk (- n step) k) (setq *oks-sink* n))))

(deftest optional-key-self-framed.frame-list-after-exits
  (let ((base (%oks-depth)))
    (list (handler-case (%oks-throw-at 30 :error) (error () (- (%oks-depth) base)))
          (handler-case (%oks-throw-at 30 :error 2) (error () (- (%oks-depth) base)))
          (catch '%oks-tag (%oks-throw-at 30 :throw))
          (- (%oks-depth) base)
          (handler-case (%oks-kthrow-at 30) (error () (- (%oks-depth) base)))
          (catch '%oks-tag (%oks-kthrow-at 30 :kind :throw :step 3))
          (- (%oks-depth) base)
          ;; 21 %OKS-WALK frames; the lambda has none
          (block out (%oks-walk 20 (lambda () (return-from out (- (%oks-depth) base)))))
          (- (%oks-depth) base)
          (let ((i 0))
            (tagbody again (incf i) (when (< i 3) (%oks-walk 10 (lambda () (go again)) 2)))
            i)
          (restart-case (%oks-walk 10 (lambda () (invoke-restart 'oks-out)))
            (oks-out () :restarted))
          (- (%oks-depth) base)))
  (0 0 :thrown 0 0 :thrown 0 21 0 3 :restarted 0))

(deftest optional-key-self-framed.frame-list-after-an-interpreted-handler
  (let ((base (%oks-depth)))
    (let ((dotcl:*evaluator-mode* :interpret))
      (list (- (eval '(handler-case (%oks-throw-at 30 :error) (error () (%oks-depth)))) base)
            (- (eval '(progn (catch '%oks-tag (%oks-kthrow-at 30 :kind :throw)) (%oks-depth))) base)
            (- (%oks-depth) base))))
  (0 0 0))

;;; --- C# that catches and carries on ------------------------------------------

(defvar *oks-cb-log* nil)
(defun %oks-cb (a &optional (b 1) (c 2))
  (case a
    (1 (%oks-throw-at 5 :error b))
    (3 (throw '%oks-tag :thrown)))
  (push (list (%oks-depth) b c) *oks-cb-log*)
  a)
(defun %oks-kcb (a &key (b 1))
  (case a
    (1 (%oks-kthrow-at 5 :step b))
    (3 (throw '%oks-tag :thrown)))
  (push (list (%oks-depth) b) *oks-cb-log*)
  a)

(defun %oks-for-each (fn &rest xs)
  (let ((l (dotnet:new "System.Collections.Generic.List`1[System.Int32]"))
        (*error-output* (make-broadcast-stream)))
    (dolist (x xs) (dotnet:invoke l "Add" x))
    (catch '%oks-tag
      (dotnet:invoke l "ForEach" (dotnet:make-delegate "System.Action`1[System.Int32]" fn)))))

(deftest optional-key-self-framed.frame-list-after-a-callback-error-is-contained
  (let ((base (%oks-depth)))
    (flet ((run (fn)
             (setq *oks-cb-log* nil)
             (%oks-for-each fn 2)
             (let ((clean *oks-cb-log*))
               (setq *oks-cb-log* nil)
               (list (%oks-for-each fn 1 2 3)
                     (equal *oks-cb-log* clean)
                     (- (%oks-depth) base)))))
      (list (run #'%oks-cb) (run #'%oks-kcb))))
  ((:thrown t 0) (:thrown t 0)))

;;; --- tail calls and redefinition -------------------------------------------

(defun %oks-count (n &optional (acc 0))
  (if (= n 0) acc (%oks-count (1- n) (1+ acc))))
(defun %oks-ev (n &optional flag) (if (= n 0) (not flag) (%oks-od (1- n))))
(defun %oks-od (n &key flag) (if (= n 0) flag (%oks-ev (1- n) nil)))

(deftest optional-key-self-framed.tail-calls-run-in-constant-space
  (list (%oks-count 1000000)
        (%oks-ev 1000000)
        (%oks-ev 1000001)
        (funcall #'%oks-od 1000001 :flag t))
  (1000000 t nil t))

(defun %oks-redef (x &optional (y :one)) (list x y))
(defun %oks-redef-caller (x) (values (%oks-redef x)))
(defun %oks-redef-caller2 (x) (values (%oks-redef x :two)))

(deftest optional-key-self-framed.redefinition-is-seen-by-callers
  (let ((r1 (list (%oks-redef-caller 1) (%oks-redef-caller2 1))))
    (eval '(defun %oks-redef (x &optional (y :three) z) (list x y z)))
    (let ((r2 (list (%oks-redef-caller 2) (%oks-redef-caller2 2))))
      (eval '(defun %oks-redef (x &rest r) (list* :rest x r)))
      (list r1 r2 (list (%oks-redef-caller 3) (%oks-redef-caller2 3)))))
  (((1 :one) (1 :two)) ((2 :three nil) (2 :two nil)) ((:rest 3) (:rest 3 :two))))
