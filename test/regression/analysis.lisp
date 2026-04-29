;;; Variable analysis regression tests
;;; Tests for free-var, mutation, and capture analysis correctness.
;;; These tests verify that the compiler correctly identifies:
;;; - Free variables for closure capture
;;; - Mutated variables for boxing
;;; - Captured variables across lambda boundaries

;;; ============================================================
;;; Free variable analysis: let/let* scoping
;;; ============================================================

;;; let* progressive scoping: later init forms see earlier bindings
(deftest analysis-let*-progressive
  (let ((x 10))
    (funcall
     (lambda ()
       (let* ((a x)     ; captures x
              (b (+ a 1)))  ; uses a, not free
         b))))
  11)

;;; let parallel scoping: init forms don't see sibling bindings
(deftest analysis-let-parallel
  (let ((x 5))
    (let ((a x)
          (b (lambda () x)))  ; b captures x, not a
      (funcall b)))
  5)

;;; ============================================================
;;; Free variable analysis: block/return-from capture
;;; ============================================================

;;; return-from inside closure must capture block tag
(deftest analysis-block-capture
  (block outer
    (funcall (lambda () (return-from outer 42)))
    99)
  42)

;;; Nested blocks: inner closure captures outer block
(deftest analysis-block-nested
  (block outer
    (block inner
      (funcall (lambda () (return-from outer 100)))
      200)
    300)
  100)

;;; ============================================================
;;; Free variable analysis: handler-case
;;; ============================================================

;;; handler-case with var binding in clause
(deftest analysis-handler-case-var
  (let ((x 10))
    (handler-case
        (funcall (lambda () (+ x (error "boom"))))
      (error (e)
        (declare (ignore e))
        x)))
  10)

;;; ============================================================
;;; Free variable analysis: flet/labels
;;; ============================================================

;;; flet function body captures outer variable
(deftest analysis-flet-capture
  (let ((x 42))
    (flet ((get-x () x))
      (get-x)))
  42)

;;; labels mutual recursion with capture
(deftest analysis-labels-mutual-capture
  (let ((result nil))
    (labels ((even-p (n)
               (if (= n 0) t (odd-p (- n 1))))
             (odd-p (n)
               (if (= n 0) nil (even-p (- n 1)))))
      (push (even-p 4) result)
      (push (odd-p 3) result))
    result)
  (t t))

;;; labels function captured in closure
(deftest analysis-labels-in-closure
  (let ((x 10))
    (labels ((add-x (n) (+ n x)))
      (funcall (lambda () (add-x 5)))))
  15)

;;; ============================================================
;;; Mutation analysis: various mutation forms
;;; ============================================================

;;; setq mutation captured in closure
(deftest analysis-setq-captured
  (let ((n 0))
    (let ((inc (lambda () (setq n (1+ n)))))
      (funcall inc)
      (funcall inc)
      n))
  2)

;;; incf/decf mutation
(deftest analysis-incf-captured
  (let ((n 0))
    (let ((inc (lambda () (incf n))))
      (funcall inc)
      (funcall inc)
      (funcall inc)
      n))
  3)

;;; push mutation
(deftest analysis-push-captured
  (let ((lst nil))
    (let ((pusher (lambda (x) (push x lst))))
      (funcall pusher 1)
      (funcall pusher 2)
      (funcall pusher 3)
      lst))
  (3 2 1))

;;; ============================================================
;;; Capture analysis: variables used inside nested lambda
;;; ============================================================

;;; Variable used in lambda inside let
(deftest analysis-capture-in-let
  (let ((x 1))
    (let ((f nil))
      (setq f (lambda () x))
      (funcall f)))
  1)

;;; Variable used in lambda inside flet body
(deftest analysis-capture-in-flet
  (let ((x 10))
    (flet ((make-getter () (lambda () x)))
      (funcall (make-getter))))
  10)

;;; ============================================================
;;; Combined: mutation + capture (requires boxing)
;;; ============================================================

;;; Mutable variable captured across multiple closures
(deftest analysis-box-shared
  (let ((count 0))
    (let ((inc (lambda () (incf count)))
          (dec (lambda () (decf count)))
          (get (lambda () count)))
      (funcall inc)
      (funcall inc)
      (funcall inc)
      (funcall dec)
      (funcall get)))
  2)

;;; Mutation in outer scope, read in inner closure
(deftest analysis-box-read-after-write
  (let ((x 0))
    (let ((reader (lambda () x)))
      (setq x 42)
      (funcall reader)))
  42)

;;; ============================================================
;;; Deep nesting (stress test for stack depth)
;;; ============================================================

;;; 10-level nested let with closure capture from outermost
(deftest analysis-deep-let-capture
  (let ((x 1))
    (let ((a (+ x 1)))
      (let ((b (+ a 1)))
        (let ((c (+ b 1)))
          (let ((d (+ c 1)))
            (let ((e (+ d 1)))
              (let ((f (+ e 1)))
                (let ((g (+ f 1)))
                  (let ((h (+ g 1)))
                    (let ((i (+ h 1)))
                      (funcall (lambda () (+ x i)))))))))))))
  11)

;;; Nested lambdas capturing from different levels
(deftest analysis-nested-lambda-capture
  (let ((x 1))
    (funcall
     (lambda ()
       (let ((y 2))
         (funcall
          (lambda ()
            (let ((z 3))
              (funcall
               (lambda ()
                 (+ x y z))))))))))
  6)

;;; ============================================================
;;; handler-bind with closure
;;; ============================================================

(deftest analysis-handler-bind-capture
  (let ((caught nil))
    (handler-bind ((error (lambda (e)
                            (setq caught (princ-to-string e))
                            (invoke-restart 'continue))))
      (restart-case
          (error "test-error")
        (continue () nil)))
    (stringp caught))
  t)

;;; ============================================================
;;; restart-case with closure capture
;;; ============================================================

(deftest analysis-restart-case-capture
  (let ((x 42))
    (restart-case
        (funcall (lambda () (invoke-restart 'use-x)))
      (use-x () x)))
  42)

;;; ============================================================
;;; macrolet: expansion should be analyzed correctly
;;; ============================================================

(deftest analysis-macrolet-capture
  (let ((x 10))
    (macrolet ((get-x () 'x))
      (funcall (lambda () (get-x)))))
  10)

;;; ============================================================
;;; Deep nesting stress test (iterative analysis, #18)
;;; ============================================================

;;; Generate deeply nested lets with a closure capture at the bottom.
;;; Tests that the iterative worklist analysis correctly handles
;;; deep nesting without the old recursion depth limit.
(deftest analysis-deep-nesting-iterative
  (let ((x 42))
    ;; Build nested expression at macroexpand time
    (macrolet ((make-deep-nest (depth)
                 (let ((form '(funcall (lambda () x))))
                   (dotimes (i depth)
                     (let ((vname (intern (format nil "V~A" i))))
                       (setf form `(let ((,vname ,i)) ,form))))
                   form)))
      (make-deep-nest 150)))
  42)
