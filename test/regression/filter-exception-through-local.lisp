;;; A filter block must move the exception off the evaluation stack before it
;;; pushes anything else. Mono's interpreter (browser-wasm), until it tiers a
;;; method up, passes a wrong value for a call argument taken directly from the
;;; exception slot, so every non-local exit out of a fasl function failed there.

(defun fx-filter-heads (form)
  (let ((heads '()))
    (labels ((walk (x)
               (loop for (a . rest) on x
                     do (when (and (consp a) (eq (car a) :begin-filter-block))
                          (let ((next (find-if (lambda (i) (not (and (consp i) (eq (car i) :declare-local))))
                                               rest)))
                            (push (if (consp next) (car next) next) heads)))
                        (when (consp a) (walk a)))))
      (walk (dotcl.cil-compiler::compile-toplevel form)))
    (remove-duplicates heads)))

(deftest-compiled-only filter-exception-through-local
  (list (fx-filter-heads '(lambda (f) (block b (funcall f (lambda () (return-from b 1))))))
        (fx-filter-heads '(lambda (f) (catch 'x (funcall f))))
        (fx-filter-heads '(lambda (f) (tagbody (funcall f (lambda () (go out))) out)))
        (fx-filter-heads '(lambda (f) (handler-case (funcall f) (error () 1))))
        (fx-filter-heads '(lambda (f) (restart-case (funcall f) (rr () 1)))))
  ((:stloc) (:stloc) (:stloc) (:stloc) (:stloc)))
