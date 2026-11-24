;;; A LABELS whose members call each other only through a macro is still nested as FLETs: the
;;; order of the nest looks at the macroexpansion of each definition, so a callee that a macro
;;; hides is bound before its caller. Before, the first expansion that called a later member
;;; demoted the group to closures (about 240 bytes per member on every entry).

(defvar *lmh-counter* 0)
(defmacro lmh-call (name arg) (incf *lmh-counter*) `(,name ,arg))

(defun %lmh-hidden (x)
  (declare (fixnum x))
  (let ((n 0))
    (declare (fixnum n))
    (macrolet ((call-late (m) `(late ,m)))
      (labels ((early (m) (call-late m))
               (late (m) (incf n m)))
        (early x)
        n))))

(defun %lmh-global-macro (x)
  (declare (fixnum x))
  (let ((n 0))
    (declare (fixnum n))
    (labels ((first-fn (m) (lmh-call second-fn m))
             (second-fn (m) (lmh-call third-fn m))
             (third-fn (m) (incf n m)))
      (first-fn x)
      n)))

(defun %lmh-cycle (x)
  (declare (fixnum x))
  (let ((n 0))
    (declare (fixnum n))
    (macrolet ((call-b (m) `(b ,m)) (call-a (m) `(a ,m)))
      (labels ((a (m) (if (> m 0) (call-b (1- m)) (incf n 100)))
               (b (m) (incf n m) (call-a m)))
        (a x)
        n))))

(defmacro lmh-twice (x) `(* 2 ,x))

(defun %lmh-hides-macro (x)
  (labels ((lmh-twice (m) (* m 10))
           (g (y) (lmh-twice y)))
    (g x)))

#+dotcl
(defun %lmh-bytes (thunk)
  (funcall thunk)
  (let ((a0 (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread")))
    (dotimes (i 1000) (funcall thunk))
    (/ (- (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread") a0) 1000)))

(deftest labels-macro-hidden-call.values
  (list (%lmh-hidden 5) (%lmh-global-macro 7) (%lmh-cycle 3) (%lmh-hides-macro 4))
  (5 7 103 40))

#+dotcl
(deftest-compiled-only labels-macro-hidden-call.no-allocation
  (list (< (%lmh-bytes (lambda () (%lmh-hidden 5))) 16)
        (< (%lmh-bytes (lambda () (%lmh-global-macro 7))) 16))
  (t t))

(deftest-emitting-only labels-macro-hidden-call.macro-runs-once
  (let ((before *lmh-counter*)
        (f (compile nil '(lambda (x)
                          (let ((n 0))
                            (declare (fixnum n))
                            (labels ((one (m) (lmh-call two m))
                                     (two (m) (incf n m)))
                              (one x)
                              n))))))
    (list (funcall f 3) (- *lmh-counter* before)))
  (3 1))
