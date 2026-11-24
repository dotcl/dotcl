;;; regression: skip-under-interp
;;; Literals and call-site caches of in-memory compiled code live in slots of
;;; the global constant pool that belong to the compilation unit: when no code
;;; of the unit can run any more, the slots are cleared and reused. These tests
;;; check that dropped code gives its slots back, and that code that is still
;;; reachable (a function, a closure, a function that is running) keeps its
;;; literals through garbage collections and slot reuse.

(defun %udc-data () (nth 2 (dotcl:emit-pool-stats)))
(defun %udc-gc () (dotcl:gc) (dotcl:gc) (dotcl:gc))

(defun %udc-make (i)
  (compile nil `(lambda () (list ',(list i (* i 1000003) (format nil "s~a" i)) 'k))))

(defun %udc-expect (i) (list (list i (* i 1000003) (format nil "s~a" i)) 'k))

;; Compiled lambdas that are dropped give their literal slots back: a second
;; batch of the same size does not grow the pool.
(deftest-emitting-only unit-data-constants.dropped-literals-reclaimed
  ;; A collection can run late (finalizers, other threads' garbage), so the
  ;; growth is judged over up to four batches: dropped code must give its
  ;; slots back by the time the pool has seen a few batches, not on the first.
  (flet ((batch () (dotimes (i 300) (funcall (%udc-make i)))))
    (batch) (%udc-gc)
    (loop repeat 4
          for p1 = (%udc-data)
          do (batch) (%udc-gc)
          thereis (< (- (%udc-data) p1) 150)))
  t)

;; Functions that are kept answer with their own literal after the pool has
;; been collected and slots reused many times.
(deftest-emitting-only unit-data-constants.live-functions-keep-literals
  (let ((keep (loop for i below 400 collect (cons i (%udc-make i)))))
    (dotimes (round 3)
      (dotimes (i 300) (funcall (%udc-make (+ 5000 i))))
      (%udc-gc))
    (every (lambda (p) (equal (funcall (cdr p)) (%udc-expect (car p)))) keep))
  t)

;; A closure outlives the function that made it.
(deftest-emitting-only unit-data-constants.closure-keeps-literal
  (let ((c (let ((maker (compile nil '(lambda (n) (lambda () (list n '(x y z) "str")))))) 
             (funcall maker 3))))
    (dotimes (round 2)
      (dotimes (i 200) (funcall (%udc-make i)))
      (%udc-gc))
    (funcall c))
  (3 (x y z) "str"))

;; A function that is running while it is redefined, and the pool is collected
;; and reused, still reads its own literals afterwards.
(defvar *udc-cb* nil)
(defun %udc-run () (let ((x '(1 2 3 :lit))) (funcall *udc-cb*) (list x '(a b :after))))

(deftest-emitting-only unit-data-constants.running-function-keeps-literals
  (progn
    (setf *udc-cb* (lambda ()
                     (eval '(defun %udc-run () :redefined '(other literal)))
                     (dotimes (i 200) (funcall (%udc-make i)))
                     (%udc-gc)
                     (dotimes (i 200) (funcall (%udc-make i)))))
    (list (%udc-run) (%udc-run)))
  (((1 2 3 :lit) (a b :after)) (other literal)))

;; Threads compile and call functions with literals while the main thread
;; collects.
(deftest-emitting-only unit-data-constants.threads
  (let* ((ok t)
         (threads (loop for k below 4
                        collect (let ((k k))
                                  (dotcl:make-thread
                                   (lambda ()
                                     (dotimes (i 150)
                                       (let ((n (+ (* k 1000) i)))
                                         (unless (equal (funcall (%udc-make n)) (%udc-expect n))
                                           (setq ok nil))))
                                     ok))))))
    (dotimes (i 5) (%udc-gc))
    (mapc #'dotcl:thread-join threads)
    ok)
  t)
