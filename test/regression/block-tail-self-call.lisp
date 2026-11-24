;;; A self call in the tail of a BLOCK that is the tail of the function body (a COND of the
;;; NEW-LET shape expands to one) loops instead of recursing: a million levels deep does not
;;; overflow the stack, and the values are the same.

(defun %bts-down (n acc)
  (block b
    (if (< n 0) (return-from b (values acc :done)))
    (return-from b (progn (if (evenp n) (%bts-down (1- n) (1+ acc)) (%bts-down (1- n) acc))))))

(defun %bts-fall (n)
  (block b
    (when (zerop n) (return-from b :zero))
    (%bts-fall (1- n))))

(defun %bts-nontail (n)
  (block b
    (if (zerop n) (return-from b 0))
    (1+ (%bts-nontail (1- n)))))

(defun %bts-closure-exit (n)
  (block b
    (when (zerop n) (return-from b :end))
    (funcall (lambda () (when (= n 3) (return-from b :early))))
    (%bts-closure-exit (1- n))))

(deftest block-tail-self-call.values
  (list (multiple-value-list (%bts-down 10 0))
        (multiple-value-list (%bts-down 1000 0))
        (%bts-fall 1000)
        (%bts-nontail 100)
        (%bts-closure-exit 10)
        (%bts-closure-exit 2))
  ((6 :done) (501 :done) :zero 100 :early :end))

;; The depth only the loop survives: the evaluator does not turn the call into one.
(deftest-compiled-only block-tail-self-call.deep
  (list (multiple-value-list (%bts-down 1000000 0))
        (%bts-fall 1000000))
  ((500001 :done) :zero))
