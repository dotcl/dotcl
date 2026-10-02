;;; A function loaded from a FASL still knows how it is called.
;;;
;;; The lambda list a DEFUN was written with is display information -- autodoc,
;;; DESCRIBE, an editor's hover -- and it travelled only in the .sil path. An
;;; image started from a core, which is what an installed dotcl runs, answered
;;; NIL for every function baked into it. These tests run in both, so they are
;;; where that difference shows up.

(deftest fll-standard-function
  (mapcar (lambda (x) (string-downcase (symbol-name x)))
          (dotcl:function-lambda-list (quote acons)))
  ("key" "datum" "alist"))

;;; The FASL carries the names uninterned, as a portable lambda list makes
;;; them. Reading it back gives each the symbol of that name accessible in the
;;; package of the function's own name, and interns nothing: ACONS's parameter
;;; names are not symbols of COMMON-LISP, so they stay uninterned.
(deftest fll-names-are-uninterned
  (every (lambda (x) (or (member x lambda-list-keywords)
                         (and (symbolp x) (null (symbol-package x)))))
         (dotcl:function-lambda-list 'acons))
  t)

;;; The keywords stay themselves rather than becoming uninterned lookalikes.
(deftest fll-lambda-list-keywords
  (let ((ll (dotcl:function-lambda-list (quote =))))
    (list (length ll) (and (member (quote &rest) ll) t)))
  (3 t))

;;; The second value says whether one was recorded at all, which is not the same
;;; question as whether the list is empty.
(deftest fll-second-value
  (multiple-value-bind (ll found) (dotcl:function-lambda-list 'acons)
    (list (and ll t) found))
  (t t))

;;; Asking twice gives the same list rather than a fresh read each time.
(deftest fll-parsed-once
  (eq (dotcl:function-lambda-list 'acons) (dotcl:function-lambda-list 'acons))
  t)
