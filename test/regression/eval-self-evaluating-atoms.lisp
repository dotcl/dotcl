;;; Every atom other than a symbol evaluates to itself (CLHS 3.1.2.1.3). EVAL
;;; answers such a form directly: compiling it would build a one-shot method
;;; whose only job is to return the literal.

(deftest eval-self-evaluating-atoms.identity
  (let* ((p #p"/tmp/eval-atom/")
         (v (vector 1 2 3))
         (h (make-hash-table))
         (bits #*1011)
         (c (complex 1 2)))
    (list (eq (eval p) p)
          (eq (eval v) v)
          (eq (eval h) h)
          (eq (eval bits) bits)
          (eql (eval c) c)
          (eq (eval #'car) #'car)))
  (t t t t t t))

(defstruct eval-atom-s a)

(deftest eval-self-evaluating-atoms.structure
  (let ((s (make-eval-atom-s :a 1)))
    (eq (eval s) s))
  t)

;; No code is generated for such a form: the JIT count on this thread stays
;; flat over many evaluations.
(deftest eval-self-evaluating-atoms.no-code
  (let ((p #p"/tmp/eval-atom-nocode/"))
    (flet ((jitted ()
             (dotnet:static "System.Runtime.JitInfo" "GetCompiledMethodCount" t)))
      (eval p)
      (let ((before (jitted)))
        (dotimes (i 300) (eval p))
        (< (- (jitted) before) 100))))
  t)
