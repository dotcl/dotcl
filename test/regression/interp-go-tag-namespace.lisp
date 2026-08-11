;;; Go tags live in their own namespace (CLHS 3.1.1): a tag may share its name
;;; with a variable, a function, or a block without disturbing any of them.
;;;
;;; The interpreter pushed tags onto the same alist ENV as variables, keyed by
;;; the tag symbol. That was not merely a shadowing bug — it was destructive:
;;;
;;;   (let ((even nil))
;;;     (dotimes (i 8) ... (go even) ... even (push i even) ...))
;;;
;;; the tag EVEN and the variable EVEN shared one entry, so (push i even) — a
;;; SETQ — overwrote the GO-TARGET with a list, and the NEXT (go even) failed
;;; with "%mini-eval: go tag EVEN not found". The first iteration worked, so the
;;; failure only appeared once the tag was branched to twice.
;;; (ansi-test DOTIMES.12 and the DO / DOLIST / TAGBODY tests of that shape.)
;;;
;;; Both evaluator paths are asserted by binding dotcl:*evaluator-mode* around
;;; the EVAL, so this runs under the ordinary compiled harness.

(defun %gtn (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (multiple-value-list (eval form))
      (error (e) (list :error (princ-to-string e))))))

;;; The exact ansi-test DOTIMES.12 shape: tag EVEN collides with variable EVEN,
;;; and the tag is reached on every even iteration (so it must survive the SETQ).
(defparameter %gtn-dotimes
  '(let ((even nil) (odd nil))
     (dotimes (i 8 (values (reverse even) (reverse odd)))
       (when (evenp i) (go even))
       (push i odd)
       (go done)
       even
       (push i even)
       done)))

(deftest interp-go-tag-namespace.dotimes-compile
  (%gtn :compile %gtn-dotimes)
  ((0 2 4 6) (1 3 5 7)))

(deftest interp-go-tag-namespace.dotimes-interpret
  (%gtn :interpret %gtn-dotimes)
  ((0 2 4 6) (1 3 5 7)))

;;; Plain TAGBODY, tag colliding with a variable that is assigned at the tag.
(defparameter %gtn-tagbody
  '(let ((acc nil) (n 0))
     (tagbody
       top
       (when (>= n 3) (go acc))
       (push n acc)
       (incf n)
       (go top)
       acc
       (push :done acc))
     (reverse acc)))

(deftest interp-go-tag-namespace.tagbody-compile
  (%gtn :compile %gtn-tagbody)
  ((0 1 2 :done)))

(deftest interp-go-tag-namespace.tagbody-interpret
  (%gtn :interpret %gtn-tagbody)
  ((0 1 2 :done)))

;;; DOLIST with the same collision.
(defparameter %gtn-dolist
  '(let ((skip nil) (kept nil))
     (dolist (x '(1 2 3 4) (list (reverse kept) (reverse skip)))
       (when (evenp x) (go skip))
       (push x kept)
       (go next)
       skip
       (push x skip)
       next)))

(deftest interp-go-tag-namespace.dolist-interpret
  (%gtn :interpret %gtn-dolist)
  (((1 3) (2 4))))

;;; The variable must still be readable and writable through its OWN binding —
;;; the collision used to route reads of EVEN to the tag entry too.
(deftest interp-go-tag-namespace.variable-unharmed-interpret
  (%gtn :interpret '(let ((tag 10))
                      (tagbody
                        (setq tag (+ tag 1))
                        (go tag)
                        (setq tag :unreached)
                        tag
                        (setq tag (* tag 2)))
                      tag))
  (22))

;;; Nested tagbodies: the inner tag of the same name wins, and the outer one is
;;; still reachable after the inner tagbody exits.
(deftest interp-go-tag-namespace.nested-shadowing-interpret
  (%gtn :interpret '(let ((log nil))
                      (tagbody
                        (tagbody
                          (go a)
                          (push :inner-skipped log)
                          a
                          (push :inner log))
                        (go a)
                        (push :outer-skipped log)
                        a
                        (push :outer log))
                      (reverse log)))
  ((:inner :outer)))

;;; A go tag named like a FUNCTION binding must not disturb it either.
(deftest interp-go-tag-namespace.flet-name-collision-interpret
  (%gtn :interpret '(flet ((f () :called))
                      (let ((r nil))
                        (tagbody
                          (go f)
                          (setq r :skipped)
                          f
                          (setq r (f)))
                        r)))
  (:called))
