;;; RETURN-FROM in the tree-walk evaluator: where its value reaches the BLOCK
;;; through the forms in between, it travels back as a value instead of being
;;; thrown. Whatever route it takes, the BLOCK gets the values and the forms in
;;; between run as CL says.

(deftest interp-return-from.values-through-let-and-if
  (multiple-value-list
   (block b (let ((x 1)) (if x (return-from b (values 1 2 3)) :no))))
  (1 2 3))

(deftest interp-return-from.no-values
  (multiple-value-list (block b (progn (return-from b (values)) :no)))
  nil)

(deftest interp-return-from.skips-the-rest-of-the-body
  (let ((log '()))
    (list (block b (push 1 log) (return-from b :out) (push 2 log))
          log))
  (:out (1)))

;; An inner block of the same name is a different block.
(deftest interp-return-from.outer-block-of-the-same-name
  (block b (list (block b (return-from b :inner)) :after))
  (:inner :after))

(deftest interp-return-from.through-an-inner-block
  (block outer (block inner (return-from outer :outer)) :no)
  :outer)

;; Out of a TAGBODY, and a GO out of a BLOCK to an enclosing TAGBODY.
(deftest interp-return-from.out-of-tagbody
  (block b (tagbody top (return-from b :out) (go top)))
  :out)

(deftest interp-return-from.go-out-of-block
  (let ((n 0))
    (tagbody
     top
       (block b (incf n) (when (< n 3) (go top)) (return-from b nil)))
    n)
  3)

;; Across UNWIND-PROTECT the cleanup runs before the block returns.
(deftest interp-return-from.unwind-protect-cleanup
  (let ((log '()))
    (list (block b (unwind-protect (return-from b :out) (push :cleanup log)))
          log))
  (:out (:cleanup)))

;; From an argument position (thrown) and from a closure called later.
(deftest interp-return-from.from-an-argument
  (block b (list 1 (return-from b :arg) 3))
  :arg)

(deftest interp-return-from.from-a-closure
  (block b (mapc (lambda (x) (when (= x 2) (return-from b x))) '(1 2 3)) :no)
  2)

;; DOTIMES and LOOP leave their block by RETURN-FROM on every exit.
(deftest interp-return-from.loops
  (list (dotimes (i 5 i))
        (loop for i from 0 when (= i 4) return (* i 10))
        (block nil (dolist (x '(1 2 3)) (when (= x 2) (return (values x :two))))))
  (5 40 2))
