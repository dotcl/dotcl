;;; A BLOCK whose value is discarded is first compiled without its binding; a macro that
;;; expands into a RETURN-FROM to it (as TRIVIA's MATCH* does, one block per pattern
;;; level) abandons that attempt and the block is compiled again with its binding.
;;; Every enclosing block used to repeat its own attempt after that, so n nested blocks
;;; took 2^n compiles (a 12-pattern MATCH* clause did not finish in 15 minutes).

(defmacro %tbn-next () '(return-from clause nil))

(defmacro %tbn-nest (n)
  (if (zerop n)
      '(%tbn-next)
      `(block whole
         (block clause
           (let ((it 1))
             (declare (ignorable it))
             (return-from whole (locally (%tbn-nest ,(1- n))))))
         (let ((it2 2))
           (declare (ignorable it2))
           (return-from whole (locally (%tbn-next)))))))

(deftest-emitting-only transparent-block-nested-compile-time.value
  (funcall (compile nil '(lambda () (progn (block clause (%tbn-nest 3)) 5))))
  5)

(deftest-emitting-only transparent-block-nested-compile-time.deep-is-fast
  (let ((start (get-internal-real-time)))
    (funcall (compile nil '(lambda () (block clause (%tbn-nest 17)))))
    (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 20))
  t)
