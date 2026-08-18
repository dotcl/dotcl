;;; An oversized LET body is rewritten into closure chunks so it does not
;;; compile to one enormous method. The rewrite happens before the LET's
;;; capture/mutation analysis, so a variable the chunks assign to gets boxed and
;;; every chunk shares the one cell. These tests pin that sharing, plus the
;;; things a closure boundary could otherwise break: non-local exit out of a
;;; chunk, multiple values from the tail form, and let* init ordering.
;;;
;;; Each body is over *progn-chunk-threshold* (256) forms, so it is chunked;
;;; before the rewrite moved ahead of the analysis these bodies were refused
;;; (the variable was a plain slot a closure would have copied) and compiled
;;; into a single method with tens of thousands of locals.

(defmacro %lc-sum-body (n)
  `(let ((acc 0))
     ,@(loop repeat n collect '(setq acc (+ acc 1)))
     acc))

(deftest let-chunked-body-mutation-shares-one-cell
  (%lc-sum-body 300)
  300)

;;; A closure made in the body must see the assignments the later chunks make:
;;; the chunks and the closure have to share the same boxed cell, not copies.
(defmacro %lc-closure-body (n)
  `(let ((acc 0) (fn nil))
     (setq fn (lambda () acc))
     ,@(loop repeat n collect '(setq acc (+ acc 1)))
     (funcall fn)))

(deftest let-chunked-body-closure-sees-later-writes
  (%lc-closure-body 300)
  300)

;;; RETURN-FROM out of the middle of a chunk crosses the closure boundary.
(defmacro %lc-return-body (n)
  `(block done
     (let ((acc 0))
       ,@(loop for i from 1 to n
               collect (if (= i 150)
                           '(when (> acc 100) (return-from done (list :early acc)))
                           '(setq acc (+ acc 1))))
       (list :end acc))))

(deftest let-chunked-body-non-local-exit
  (%lc-return-body 300)
  (:early 149))

;;; The tail form stays inline, so it keeps its multiple values.
(defmacro %lc-values-body (n)
  `(let ((acc 0))
     ,@(loop repeat n collect '(setq acc (+ acc 1)))
     (values acc :second)))

(deftest let-chunked-body-tail-form-keeps-values
  (multiple-value-list (%lc-values-body 300))
  (300 :second))

;;; LET* : inits run in order and later assignments still land on the same cell.
(defmacro %lc-let*-body (n)
  `(let* ((a 1) (b (+ a 1)))
     ,@(loop repeat n collect '(setq b (+ b 1)))
     (list a b)))

(deftest let-chunked-body-sequential-bindings
  (%lc-let*-body 300)
  (1 302))

;;; A raw native slot must NOT be chunked — capturing one would emit invalid IL.
;;; The refusal path still has to compile and give the right answer.
(defmacro %lc-fixnum-body (n)
  `(let ((acc 0))
     (declare (type fixnum acc))
     ,@(loop repeat n collect '(setq acc (+ acc 1)))
     acc))

(deftest let-chunked-body-declared-fixnum-still-correct
  (%lc-fixnum-body 300)
  300)

;;; Special bindings in an oversized body keep dynamic-binding semantics.
(defvar *lc-special* :outer)

(defmacro %lc-special-body (n)
  `(let ((*lc-special* :inner) (acc 0))
     ,@(loop repeat n collect '(setq acc (+ acc 1)))
     (list *lc-special* acc)))

(deftest let-chunked-body-special-binding
  (list (%lc-special-body 300) *lc-special*)
  ((:inner 300) :outer))

;;; Under the threshold nothing is rewritten; behaviour is unchanged.
(deftest let-small-body-unchanged
  (let ((acc 0))
    (setq acc (+ acc 1))
    (setq acc (+ acc 1))
    acc)
  2)
