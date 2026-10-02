;;; A BLOCK used as the test of IF / WHEN, whose RETURN-FROM only appears after
;;; macroexpansion.
;;;
;;; In a test position a BLOCK that nothing returns from is compiled without its
;;; binding. Whether anything returns from it was decided on the source, where
;;; (RETURN X) is not yet (RETURN-FROM NIL X), so a RETURN in a closure passed
;;; out of the block found no block ("return-from: no block named NIL") or, with
;;; an outer block of the same name, returned from that one. SBCL's
;;; equality-constraints.lisp has this shape and stopped building SBCL with
;;; dotcl as the host.

(defun btr-call (fn) (funcall fn) nil)

(deftest block-test-return-in-closure
  (if (block nil (btr-call (lambda () (return t)))) :yes :no)
  :yes)

(deftest block-test-return-in-closure-and
  (let ((x 1))
    (when (and x (block nil (btr-call (lambda () (return t))))) :yes))
  :yes)

(deftest block-test-return-direct
  (if (block nil (return nil) t) :yes :no)
  :no)

;; With an outer BLOCK of the same name, the RETURN must still go to the inner
;; one: the outer block's value is the list, not T.
(deftest block-test-return-outer-same-name
  (block nil
    (list (if (block nil (btr-call (lambda () (return t)))) :yes :no) :after))
  (:yes :after))

(defmacro btr-leave (name) `(return-from ,name :left))

(deftest block-test-return-from-via-macro
  (if (block b (btr-call (lambda () (btr-leave b))) nil) :yes :no)
  :yes)

;; An inner BLOCK of the same name that is returned to still works.
(deftest block-test-inner-block-same-name
  (if (block nil (block nil (return 1)) nil) :yes :no)
  :no)
