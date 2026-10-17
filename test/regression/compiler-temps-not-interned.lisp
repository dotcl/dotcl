;;; The compiler's temporaries (locals and labels it makes up for a form) used
;;; to be interned in DOTCL.CIL-COMPILER, one new symbol per temporary per form
;;; compiled, so a long-running image that kept compiling grew without bound.
;;; So were the names it derives from a LABELS function, a block or a tagbody,
;;; one per gensym a macro made. Compiling must leave the compiler package's
;;; own symbols as they were.
;;; The emit-free build has no COMPILE, so the tests run where code is emitted.

(defun %cc-home-symbol-count ()
  (let ((pkg (find-package "DOTCL.CIL-COMPILER"))
        (n 0))
    (do-symbols (s pkg)
      (when (eq (symbol-package s) pkg) (incf n)))
    n))

(defun %cc-temps-forms ()
  "Forms that need many compiler temporaries. Fresh gensyms each call, so a
   compile cannot reuse anything from an earlier one."
  (let ((a (gensym "A")) (b (gensym "B")) (f (gensym "F")))
    (list
     `(lambda (x y)
        (let* ((,a (+ x 1)) (,b (* ,a y)))
          (if (> ,b 10)
              (cond ((evenp ,b) (list ,a ,b))
                    (t (multiple-value-bind (q r) (floor ,b 3) (list q r))))
              (case x (1 :one) (2 :two) (t (vector ,a ,b))))))
     `(lambda (x)
        (let ((,f (lambda (z) (+ z x))))
          (funcall ,f (funcall ,f 1))))
     `(lambda (n)
        (let ((acc 0))
          (flet ((helper (k) (+ acc k)))
            (setq acc (helper n))
            (format nil "~a ~a" acc (helper 2)))))
     ;; Names the compiler derives from the user's names: a LABELS function's
     ;; box, a block's tag and a tagbody's id, here all from fresh gensyms (as
     ;; macro expansions make them), captured by a closure.
     `(lambda (n)
        (labels ((,f (k) (if (zerop k) 0 (+ k (,f (1- k))))))
          (let ((g #',f))
            (block ,b
              (tagbody
               ,a
                 (when (> n 100) (return-from ,b :big))
                 (let ((r (funcall (lambda () (funcall g n)))))
                   (when (< r 0) (go ,a))
                   (return-from ,b (list r (mapcar (lambda (x) (,f x)) '(1 2 3)))))))))))))

(defun %cc-compile-all ()
  (dolist (form (%cc-temps-forms))
    (compile nil form)))

(deftest-emitting-only compiler-temps-not-interned.compile
  (progn
    ;; Warm-up: the first compile of a shape may load or intern things once.
    (%cc-compile-all)
    (let ((before (%cc-home-symbol-count)))
      (dotimes (i 5) (%cc-compile-all))
      (- (%cc-home-symbol-count) before)))
  0)

;;; The compiled code still works: the temporaries are named, not found by
;;; package, so being uninterned changes nothing about what runs.
(deftest-emitting-only compiler-temps-not-interned.results
  (destructuring-bind (f1 f2 f3 f4)
      (mapcar (lambda (form) (compile nil form)) (%cc-temps-forms))
    (list (funcall f1 3 4) (funcall f1 1 2) (funcall f2 10) (funcall f3 5)
          (funcall f4 4) (funcall f4 200)))
  ((4 16) :one 21 "5 7" (10 (1 3 6)) :big))

;;; Compiles on several threads at once: each compile keeps its derived names in
;;; a table of its own, so no thread sees another's, and nothing is left in the
;;; package afterwards.
(require "dotcl-thread")

(deftest-emitting-only compiler-temps-not-interned.threads
  (let* ((before (%cc-home-symbol-count))
         (results (make-array 4 :initial-element nil))
         (threads
           (loop for i below 4
                 collect (let ((i i))
                           (dotcl-thread:make-thread
                            (lambda ()
                              (dotimes (round 3)
                                (let ((f4 (compile nil (fourth (%cc-temps-forms)))))
                                  (push (funcall f4 4) (aref results i))))))))))
    (dolist (th threads) (dotcl-thread:thread-join th))
    (list (- (%cc-home-symbol-count) before)
          (every (lambda (r) (equal r '((10 (1 3 6)) (10 (1 3 6)) (10 (1 3 6)))))
                 results)))
  (0 t))

;;; The names made for gensym variables (VAR-NAME) are kept per compile: the
;;; table that holds them for calls outside a compile does not grow when the
;;; compiler meets a gensym variable, so the gensyms macros make are not kept
;;; alive by it.
(defun %cc-global-var-names-count ()
  (let ((s (find-if (lambda (s) (and (boundp s) (hash-table-p (symbol-value s))))
                    (find-all-symbols "*UNINTERNED-VAR-NAMES*"))))
    (and s (hash-table-count (symbol-value s)))))

(deftest-emitting-only compiler-temps-not-interned.gensym-var-names
  (let ((before (%cc-global-var-names-count)))
    (dotimes (i 3) (%cc-compile-all))
    (list (integerp before) (eql before (%cc-global-var-names-count))))
  (t t))
