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
;;; Memoized intrinsic free-vars : nested lambdas whose
;;; free-var sets are computed once and re-scoped by BND subtraction.
;;; Exercises correctness of the per-form %lambda-intrinsic-free cache.
;;; ============================================================

;;; Same-named variable rebound at each nesting level: the innermost
;;; reference must resolve to the NEAREST binding, so no spurious capture
;;; of an outer same-named var. If BND re-scoping were wrong the value
;;; would differ.
(deftest analysis-memo-shadow-rescope
  (let ((x 100))
    (funcall
     (lambda ()
       (let ((x 200))
         (funcall
          (lambda ()
            (let ((x 300))
              (funcall (lambda () x))))))))
    )
  300)

;;; Five-level nested lambdas each capturing a distinct outer var — the
;;; intrinsic set of each inner lambda must propagate every still-free
;;; name up through all enclosing levels.
(deftest analysis-memo-deep-multi-capture
  (let ((a 1))
    (funcall
     (lambda ()
       (let ((b 2))
         (funcall
          (lambda ()
            (let ((c 4))
              (funcall
               (lambda ()
                 (let ((d 8))
                   (funcall
                    (lambda () (+ a b c d)))))))))))))
  15)

;;; A lambda appearing in an &optional default form must have its own
;;; captures analyzed (the defaults branch of %compute-lambda-intrinsic-free).
(deftest analysis-memo-optional-default-lambda
  (let ((base 40))
    (funcall
     (lambda (&optional (f (lambda () base)))
       (funcall f))))
  40)

;;; Two sibling lambdas sharing an identical body shape but distinct conses:
;;; each is cached under its own key, so both capture correctly.
(deftest analysis-memo-sibling-lambdas
  (let ((p 3) (q 7))
    (let ((f (lambda () p))
          (g (lambda () q)))
      (+ (funcall f) (funcall g))))
  10)

;;; ============================================================
;;; Deferred free-var candidate memo : special capture
;;; names (block tag / go tag / labels fn / #'fn) must survive the
;;; *locals*-independent candidate collection + per-merge local-bound-p.
;;; ============================================================

;;; block tag captured through TWO nested lambdas (candidate must carry the
;;; %BTAG- name up and be re-filtered by local-bound-p at each level).
(deftest analysis-memo-block-deep
  (block outer
    (funcall
     (lambda ()
       (funcall
        (lambda ()
          (return-from outer 77)))))
    99)
  77)

;;; labels fn captured through TWO nested lambdas via direct call
;;; (__LABELFN_ mangled candidate).
(deftest analysis-memo-labels-deep
  (let ((base 100))
    (labels ((bump (n) (+ n base)))
      (funcall
       (lambda ()
         (funcall
          (lambda () (bump 11)))))))
  111)

;;; labels fn captured via #'fn (function-case candidate) across a lambda.
(deftest analysis-memo-sharpquote-labels
  (labels ((squ (x) (* x x)))
    (funcall (lambda () (mapcar #'squ '(1 2 3)))))
  (1 4 9))

;;; go tag captured across a lambda (tagbody var candidate).
(deftest analysis-memo-go-capture
  (let ((acc nil))
    (tagbody
       (funcall (lambda () (go skip)))
       (push 'reached acc)
     skip)
    acc)
  nil)

;;; Lisp-2: same name as both a lexical variable and a labels function,
;;; the variable captured across a lambda while #'name is used elsewhere.
;;; Exercises the function-case both-names candidate branch.
(deftest analysis-memo-lisp2-samename
  (let ((f 5))
    (labels ((f (x) (* x 10)))
      (+ (funcall (lambda () f))        ; captures the VARIABLE f = 5
         (funcall (lambda () (f 3))))))  ; calls the FUNCTION f
  35)

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
;;; Deep nesting stress test (iterative analysis)
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

;;; ============================================================
;;; Binding-name positions must not be macroexpanded
;;; ============================================================
;;; A lambda-list parameter (or LET variable) whose name happens to name a
;;; macro is a binding occurrence, not a call. The compiler's return-from
;;; scanner and mutation/capture walk must not macroexpand it: doing so fires
;;; the macro's compile-time side effects. SBCL's assembler INST macro is one
;;; real case — modarith.lisp has (labels ((commutativep (inst) ...)) ...), and
;;; INST WARNs on an undefined mnemonic, which set compile-file failure-p.

(defvar *bindpos-expand-count* 0)

(defmacro bindpos-marker (&rest args)
  (declare (ignore args))
  (incf *bindpos-expand-count*)
  ''expanded)

;; labels param named after the macro — a binding, never called.
(defun bindpos-labels (x)
  (labels ((pick (bindpos-marker) (list bindpos-marker)))
    (pick x)))

;; flet param named after the macro.
(defun bindpos-flet (x)
  (flet ((pick (bindpos-marker) (list bindpos-marker)))
    (pick x)))

;; let variable named after the macro.
(defun bindpos-let (x)
  (let ((bindpos-marker x)) (list bindpos-marker)))

;; The three functions above were compiled at file-load time. The macro must
;; not have expanded during any of those compiles.
(deftest bindpos-no-spurious-expansion
  *bindpos-expand-count*
  0)

;; And they behave correctly — the name is an ordinary binding.
(deftest bindpos-labels-result (bindpos-labels 'a) (a))
(deftest bindpos-flet-result (bindpos-flet 'b) (b))
(deftest bindpos-let-result (bindpos-let 'c) (c))

;; A form can LOOK like a binding special form without being one: a DOLIST
;; spec whose variable is named LET reads as (LET <list-form>), i.e. car LET
;; and a symbol where a binding list would be. The scanners walk unevaluated
;; subforms too, so they must tolerate that shape instead of assuming the
;; binding-list and body positions hold proper lists.
(defmacro bindpos-drop (form)
  (declare (ignore form))
  nil)

(defun bindpos-shape-let (x)
  (block b (bindpos-drop (let lets)) x))

(defun bindpos-shape-labels (x)
  (block b (bindpos-drop (labels labels)) x))

(defun bindpos-shape-flet (x)
  (block b (bindpos-drop (flet flets)) x))

(defun bindpos-shape-lambda (x)
  (block b (bindpos-drop (lambda args)) x))

(deftest bindpos-shape-let-result (bindpos-shape-let 'a) a)
(deftest bindpos-shape-labels-result (bindpos-shape-labels 'b) b)
(deftest bindpos-shape-flet-result (bindpos-shape-flet 'c) c)
(deftest bindpos-shape-lambda-result (bindpos-shape-lambda 'd) d)
