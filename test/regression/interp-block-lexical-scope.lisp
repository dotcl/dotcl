;;; BLOCK names are LEXICAL: a same-named inner BLOCK must not hide the outer one.
;;;
;;; %MINI-EVAL's BLOCK used the block-name symbol itself as the CATCH tag, which
;;; made the scope DYNAMIC. A same-named inner BLOCK then hid the outer one for
;;; everything running inside it — including a closure whose text is lexically
;;; outside.
;;;
;;;   (block done
;;;     (flet ((%f (x) (return-from done x)))
;;;       (block done (mapcar #'%f '(good bad bad))))
;;;     'bad)
;;;
;;; %f's RETURN-FROM refers to the OUTER DONE, but the throw landed on the inner
;;; one. MAPCAR simply moved on to its next element and the whole form answered
;;; BAD (ansi-test BLOCK.10).
;;;
;;; The fix gives every BLOCK entry a fresh tag recorded in its own namespace in
;;; ENV (%MINI-BLOCKS). A closure carries the ENV it was made in, so looking the
;;; tag up there is exactly the lexical rule — the same shape GO tags use.

(defun %bl (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e))))))

;;; --- the point: return to the outer block THROUGH a same-named inner one
;;; (ansi BLOCK.10)

(defparameter %bl-shadowed
  '(block done
    (flet ((%f (x) (return-from done x)))
      (block done (mapcar #'%f '(good bad bad))))
    'bad))

(deftest interp-block-lexical.shadowed-outer-compile
  (%bl :compile %bl-shadowed)
  good)

(deftest interp-block-lexical.shadowed-outer-interpret
  (%bl :interpret %bl-shadowed)
  good)

;;; --- returning from the inner block to itself is unchanged (this rejects a
;;; fix that simply always picks the outer one)

(deftest interp-block-lexical.innermost-wins-interpret
  (%bl :interpret '(block a (list (block a (return-from a :inner)) :after)))
  (:inner :after))

;;; --- a RETURN-FROM in the outer body reaches the outer block

(deftest interp-block-lexical.outer-body-interpret
  (%bl :interpret '(block a (block a 1) (return-from a :outer) :bad))
  :outer)

;;; --- BLOCK NIL / RETURN, and the macros that expand into them

(deftest interp-block-lexical.block-nil-interpret
  (%bl :interpret '(block nil (return :ok) :bad))
  :ok)

(deftest interp-block-lexical.iteration-macros-interpret
  (%bl :interpret '(list (dolist (x '(1 2 3)) (when (= x 2) (return (* x 10))))
                    (do ((i 0 (1+ i))) ((> i 5) :done) (when (= i 3) (return i)))
                    (loop for i from 1 to 5 do (when (= i 3) (return i)))))
  (20 3 3))

;;; --- the implicit block of a function (DEFUN / LABELS) still works

(deftest interp-block-lexical.function-blocks-interpret
  (%bl :interpret '(labels ((%g (n) (if (> n 2) (return-from %g :big) :small)))
                    (list (%g 1) (%g 5))))
  (:small :big))

;;; --- a RETURN-FROM introduced by a macro expansion must reach the same block.
;;; The expander runs in a different ENV, which is what a fix that puts tags in
;;; ENV is most likely to break.

(deftest interp-block-lexical.return-from-in-macroexpansion-interpret
  (%bl :interpret '(macrolet ((%m () '(return-from b :from-macro)))
                    (block b (%m) :bad)))
  :from-macro)

;;; --- multiple values pass through a block

(deftest interp-block-lexical.multiple-values-interpret
  (%bl :interpret '(multiple-value-list (block b (values 1 2 3))))
  (1 2 3))
