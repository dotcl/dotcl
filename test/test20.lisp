;; test20.lisp — closure capture regression tests (D4 bug fix)
(progn
  ;; 1. Basic free variable capture
  (let ((x 42))
    (print (funcall (lambda () x))))              ;; => 42

  ;; 2. Mutated captured variable requires boxing
  (let ((counter 0))
    (let ((inc (lambda () (setq counter (+ counter 1)))))
      (funcall inc)
      (funcall inc)
      (funcall inc)
      (print counter)))                            ;; => 3

  ;; 3. Multiple free variables
  (let ((a 10) (b 20))
    (print (funcall (lambda () (+ a b)))))         ;; => 30

  ;; 4. Nested closure captures
  (let ((x 1))
    (let ((f (lambda ()
               (let ((y 2))
                 (lambda () (+ x y))))))
      (print (funcall (funcall f)))))              ;; => 3

  ;; 5. maphash + push + lambda pattern (original bug trigger)
  (let ((keys '()))
    (let ((ht (make-hash-table)))
      (setf (gethash 'a ht) 1)
      (setf (gethash 'b ht) 2)
      (maphash (lambda (k v) (push k keys)) ht)
      (print (length keys))))                      ;; => 2

  ;; 6. let* with special variable + block init (CIL stack constraint bug)
  (defvar *test-sp20* nil)
  (let* ((*test-sp20* (block blk
                        (if t (return-from blk 99) 0))))
    (print *test-sp20*))                           ;; => 99

  ;; 7. Closure over loop variable
  (let ((fns '()))
    (dotimes (i 3)
      (let ((j i))
        (push (lambda () j) fns)))
    (print (funcall (car fns))))                   ;; => 2

  ;; 8. let* special with tagbody init (another try-block pattern)
  (defvar *test-sp20b* nil)
  (let* ((*test-sp20b* (block nil
                         (tagbody
                           (setq *test-sp20b* 77)
                           (go done)
                           done)
                         *test-sp20b*)))
    (print *test-sp20b*)))                         ;; => 77
