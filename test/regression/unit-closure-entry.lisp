;;; A closure compiled in memory (LOAD of source, EVAL, COMPILE) with up to four
;;; required parameters is called through an entry pointer, as one from a .fasl
;;; is. Its body lives in a collectible compilation unit, which the call keeps
;;; reachable until the body returns.

(defun %uce-state (f) (dotnet:static "DotCL.LispFunction" "EntryState" f))
(defun %uce-mk0 (k) (lambda () k))
(defun %uce-mk1 (k) (lambda (a) (list k a)))
(defun %uce-mk4 (k) (lambda (a b c d) (list k a b c d)))
(defun %uce-mk5 (k) (lambda (a b c d e) (list k a b c d e)))
(defun %uce-mkopt (k) (lambda (a &optional (b 2)) (list k a b)))
(defun %uce-mkmv (k) (lambda (a) (values k a)))
(defun %uce-counter () (let ((n 0)) (lambda (d) (incf n d))))
(defun %uce-lab (n) (labels ((f (k) (if (= k 0) 0 (1+ (f (1- k)))))) (f n)))
(defun %uce-mkloop ()
  (let ((f nil))
    (setq f (lambda (n) (if (zerop n) :done (funcall f (- n 1)))))
    f))
(defun %uce-call-g (x) (funcall '%uce-g x))

(deftest-compiled-only unit-closure-entry.state
  (list (%uce-state (%uce-mk0 1)) (%uce-state (%uce-mk1 1)) (%uce-state (%uce-mk4 1))
        (%uce-state (%uce-mk5 1)) (%uce-state (%uce-mkopt 1)))
  ("closure-entry" "closure-entry" "closure-entry" "none" "none"))

(deftest-compiled-only unit-closure-entry.calls
  (let ((f1 (%uce-mk1 :k)) (f4 (%uce-mk4 :k)))
    (list (funcall (%uce-mk0 :k))
          (funcall f1 1) (apply f1 '(2)) (mapcar f1 '(3 4))
          (funcall f4 1 2 3 4) (apply f4 '(1 2 3 4))
          (handler-case (funcall f1 1 2) (program-error () :program-error))
          (multiple-value-list (funcall (%uce-mkmv :k) 1))))
  (:k (:k 1) (:k 2) ((:k 3) (:k 4)) (:k 1 2 3 4) (:k 1 2 3 4) :program-error (:k 1)))

(deftest unit-closure-entry.environment
  (let ((c (%uce-counter)) (d (%uce-counter)))
    (funcall c 1) (funcall c 2) (funcall d 10)
    (list (funcall c 3) (funcall d 0)))
  (6 10))

(deftest unit-closure-entry.labels
  (%uce-lab 1000)
  1000)

;; The name is looked up at each call: a new closure under it, or TRACE's
;; wrapper, is what the next call runs.
(deftest-compiled-only unit-closure-entry.redefine-and-trace
  (let ((out (make-string-output-stream)) (r '()))
    (setf (fdefinition '%uce-g) (%uce-mk1 :a))
    (push (%uce-call-g 1) r)
    (setf (fdefinition '%uce-g) (%uce-mk1 :b))
    (push (%uce-call-g 2) r)
    (eval '(trace %uce-g))
    (push (let ((*trace-output* out)) (%uce-call-g 3)) r)
    (eval '(untrace %uce-g))
    (push (plusp (length (get-output-stream-string out))) r)
    (push (%uce-call-g 4) r)
    (nreverse r))
  ((:a 1) (:b 2) (:b 3) t (:b 4)))

;; Entered by .NET through Invoke0 on a pool thread with no frame linked: the
;; tail calls of the loop stay tail calls.
(defun %uce-run-on-pool (thunk)
  (let* ((fty (dotnet:static "System.Type" "GetType"
                             "System.Func`1[[DotCL.LispObject, DotCL.Runtime]]"))
         (task (dotnet:new "System.Threading.Tasks.Task`1[[DotCL.LispObject, DotCL.Runtime]]"
                           (dotnet:static "System.Delegate" "CreateDelegate" fty thunk "Invoke0"))))
    (dotnet:invoke task "Start")
    (dotnet:invoke task "get_Result")))

(deftest-compiled-only unit-closure-entry.tail-loop-without-frame
  (let* ((f (%uce-mkloop))
         (thunk (lambda () (handler-case (funcall f 3000000)
                             (storage-condition () :overflow)))))
    (list (%uce-state f) (%uce-state thunk) (%uce-run-on-pool thunk)))
  ("closure-entry" "closure-entry" :done))

;; The only reference to the closure is the call itself while its body collects
;; and then makes closures of its own unit.
(defun %uce-fresh (k)
  (funcall (compile nil `(lambda ()
                           (let ((k ,k))
                             (lambda (x)
                               (dotcl:gc)
                               (+ (funcall (lambda () x)) (funcall (lambda () k)))))))))

(deftest-emitting-only unit-closure-entry.collect-during-call
  (let ((r 0))
    (dotimes (i 4 r)
      (incf r (funcall (%uce-fresh i) 10))))
  46)

;; One body is entered from CallN with the function and from the delegate
;; (APPLY, MULTIPLE-VALUE-CALL) with the environment; both see the same one.
(defun %uce-acc (k) (let ((n 0)) (lambda (a b c d) (incf n (+ a b c d k)))))
(defun %uce-acc0 (k) (let ((n k)) (lambda () (incf n))))

(deftest unit-closure-entry.both-ways
  (let ((f (%uce-acc 100)) (g (%uce-acc0 5)))
    (list (funcall f 1 2 3 4) (apply f '(1 1 1 1)) (apply f 1 '(0 0 0))
          (multiple-value-call f (values 1 2) (values 3 4))
          (funcall g) (apply g '()) (mapcar (lambda (x) (declare (ignore x)) (funcall g)) '(1 2))))
  (110 214 315 425 6 7 (8 9)))
