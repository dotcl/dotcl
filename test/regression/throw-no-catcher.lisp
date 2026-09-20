;;; A THROW with no outstanding CATCH is a CONTROL-ERROR, not a dead process.
;;;
;;; CLHS 5.2: "If there is no outstanding catcher whose tag is EQ to the
;;; throw's tag, an error of type control-error is signaled." dotcl raised a
;;; .NET exception and converted it to a condition at the EVAL boundary -- which
;;; is after the HANDLER-CASE around the THROW has been unwound past, so nothing
;;; could catch it, and at the LOAD boundary there was no conversion at all: a
;;; library whose load-time THROW missed its catcher took the host process down
;;; with it (fset, and serapeum behind it).
;;;
;;; The check now happens where the throw does, so the condition is signalled in
;;; the dynamic environment that contains the handlers.

;;; ---- an unmatched throw signals ----

(deftest throw-no-catcher.handler-case-control-error
  (handler-case (throw 'tnc-nope 1)
    (control-error () :control-error)
    (error () :other-error))
  :control-error)

(defun %tnc-thrower () (throw 'tnc-nope 2))

(deftest throw-no-catcher.from-inside-a-function
  (handler-case (%tnc-thrower)
    (control-error () :control-error)
    (error () :other-error))
  :control-error)

;; The condition names the tag: without it the report says only that something
;; threw, which is what made this hard to place in a library's load.
(deftest throw-no-catcher.names-the-tag
  (handler-case (throw 'tnc-nope 3)
    (control-error (c) (and (search "TNC-NOPE" (princ-to-string c)) t)))
  t)

;; handler-bind sees it too, in the dynamic environment of the throw.
(deftest throw-no-catcher.handler-bind
  (block done
    (handler-bind ((control-error (lambda (c) (declare (ignore c))
                                    (return-from done :saw-it))))
      (throw 'tnc-nope 4))
    :not-signalled)
  :saw-it)

;; A tag that exists but is not the one thrown to is still no catcher for it.
(deftest throw-no-catcher.wrong-tag-still-signals
  (catch 'tnc-other
    (handler-case (throw 'tnc-nope 5)
      (control-error () :control-error)))
  :control-error)

;;; ---- and a matched throw is untouched ----

(deftest throw-no-catcher.plain-catch-still-works
  (catch 'tnc-tag (throw 'tnc-tag :value))
  :value)

(deftest throw-no-catcher.nested-catch-outer-tag
  (catch 'tnc-outer
    (catch 'tnc-inner
      (throw 'tnc-outer :from-inner))
    :not-reached)
  :from-inner)

;; The values of the throw's result form are the catch's values (CLHS 5.2), and
;; the check in front of the throw must not disturb them.
(deftest throw-no-catcher.multiple-values
  (multiple-value-list (catch 'tnc-mv (throw 'tnc-mv (values 1 2 3))))
  (1 2 3))

(deftest throw-no-catcher.catch-returns-body-value-when-no-throw
  (catch 'tnc-quiet :fell-through)
  :fell-through)

;; A throw out of a deep call chain still reaches its catcher.
(defun %tnc-deep (n) (if (zerop n) (throw 'tnc-deep :bottom) (%tnc-deep (1- n))))

(deftest throw-no-catcher.deep-throw
  (catch 'tnc-deep (%tnc-deep 1000))
  :bottom)

;; UNWIND-PROTECT cleanups run when the throw has a catcher.
(defvar *tnc-cleanup* nil)

(deftest throw-no-catcher.unwind-protect-on-matched-throw
  (progn
    (setq *tnc-cleanup* nil)
    (list (catch 'tnc-up
            (unwind-protect (throw 'tnc-up :thrown)
              (setq *tnc-cleanup* :ran)))
          *tnc-cleanup*))
  (:thrown :ran))

;; A catch whose tag is a fresh cons (what the compiler itself uses for its
;; internal protocols) matches by identity, not by structure.
(deftest throw-no-catcher.cons-tag-identity
  (let ((tag (list :tnc)))
    (list (catch tag (throw tag :same-object))
          (handler-case (throw (list :tnc) :different-object)
            (control-error () :control-error))))
  (:same-object :control-error))
