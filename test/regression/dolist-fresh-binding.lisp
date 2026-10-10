;;; DOLIST binds its variable afresh for each element, as SBCL, CCL and ECL do
;;; (CLHS leaves the choice to the implementation). A closure made in the body
;;; keeps the element it was made for. With one binding assigned on every
;;; iteration, every closure saw NIL once the loop ended: closure-template's
;;; TTABLE-EXTEND-PACKAGE makes one function per template name this way, and
;;; each of them looked up the template named NIL. Expected values are SBCL's.

(deftest dolist-fresh-binding.closures
  (let ((fns '()))
    (dolist (x '(1 2 3))
      (push (lambda () x) fns))
    (mapcar #'funcall fns))
  (3 2 1))

;; The variable is still NIL when the result form runs.
(deftest dolist-fresh-binding.result-sees-nil
  (dolist (x '(1 2) (list x)))
  (nil))

;; A type declared for the elements does not apply to that NIL.
(deftest dolist-fresh-binding.typed-result
  (let ((sum 0))
    (dolist (x '(1 2 3) (list x sum))
      (declare (fixnum x))
      (incf sum x)))
  (nil 6))

;; A SPECIAL declaration does, and the result form sees the special NIL.
(defvar *dolist-fresh-binding-var* :global)
(defun dolist-fresh-binding-peek () *dolist-fresh-binding-var*)
(deftest dolist-fresh-binding.special
  (let ((seen '()))
    (dolist (*dolist-fresh-binding-var* '(:a :b)
             (list (nreverse seen) (dolist-fresh-binding-peek)))
      (push (dolist-fresh-binding-peek) seen)))
  ((:a :b) nil))

;; The body is still a TAGBODY, and SETQ of the variable stays inside one
;; iteration.
(deftest dolist-fresh-binding.tags-and-setq
  (let ((out '()))
    (dolist (x '(1 2 3) (nreverse out))
      (when (= x 2) (go skip))
      (setq x (* x 10))
      (push x out)
      skip))
  (10 30))
