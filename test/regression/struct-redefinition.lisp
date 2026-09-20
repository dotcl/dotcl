;;; Redefining a DEFSTRUCT must not leave old code reading the wrong slot.
;;;
;;; A compiled call site does not name the slot it reads: DEFSTRUCT resolves the
;;; accessor to a position and the caller emits that integer. Redefining the
;;; structure with its slots in another order therefore left every caller
;;; compiled against the old definition reading a different slot -- a valid
;;; index into a valid instance, so it came back as a plausible value with no
;;; complaint. Changing only a slot's type was caught, but as a failed cast,
;;; which says nothing about what happened.
;;;
;;; Each structure now carries a layout version that changes exactly when its
;;; slot layout does, and the version travels in the same constant as the index.
;;; What the tests pin: the mismatch is reported in both directions, the report
;;; says which side is stale, re-evaluating the same definition changes nothing,
;;; and code compiled to a FASL against a later definition does not read an
;;; older instance.

;;; ---- re-evaluating the same definition ----

(defstruct srd-same (a 0 :type fixnum) (b 0 :type fixnum))

(defvar *srd-same-instance* (make-srd-same :a 1 :b 2))
(defvar *srd-same-accessor* #'srd-same-a)

;; The shape a REPL produces on every file reload. Nothing moved, so nothing
;; that was already compiled may go stale: the instance, the accessor captured
;; before the re-evaluation, and a fresh instance all keep working.
(deftest struct-redefinition.same-definition-is-a-no-op
  (progn
    (eval '(defstruct srd-same (a 0 :type fixnum) (b 0 :type fixnum)))
    (list (funcall *srd-same-accessor* *srd-same-instance*)
          (srd-same-a *srd-same-instance*)
          (srd-same-b *srd-same-instance*)
          (srd-same-a (make-srd-same :a 5 :b 6))))
  (1 1 2 5))

;;; ---- the slots change places ----

(defstruct srd-swap (a 0 :type fixnum) (b 0.0d0 :type double-float))

(defvar *srd-swap-old* (make-srd-swap :a 7 :b 1.5d0))
(defvar *srd-swap-old-a* #'srd-swap-a)
(defvar *srd-swap-old-b* #'srd-swap-b)

(defun %srd-swap-inlined-a (x) (srd-swap-a x))

(defun %srd-redefined-p (thunk)
  "The value THUNK answers, or :REPORTED when it says the structure was
   redefined. A wrong slot would come back as a number, which is the whole
   point: the test fails with the value it read."
  (handler-case (funcall thunk)
    (error (e) (if (search "was redefined" (format nil "~a" e))
                   :reported
                   (list :other (format nil "~a" e))))))

;; Swap the two slots. Everything below runs against this definition.
(eval '(defstruct srd-swap (b 0.0d0 :type double-float) (a 0 :type fixnum)))

(defvar *srd-swap-new* (make-srd-swap :a 7 :b 1.5d0))

;; An accessor from before the swap, applied to an instance built after it.
(deftest struct-redefinition.old-accessor-new-instance
  (list (%srd-redefined-p (lambda () (funcall *srd-swap-old-a* *srd-swap-new*)))
        (%srd-redefined-p (lambda () (funcall *srd-swap-old-b* *srd-swap-new*))))
  (:reported :reported))

;; The current accessor, applied to an instance built before the swap.
(deftest struct-redefinition.new-accessor-old-instance
  (list (%srd-redefined-p (lambda () (srd-swap-a *srd-swap-old*)))
        (%srd-redefined-p (lambda () (srd-swap-b *srd-swap-old*))))
  (:reported :reported))

;; A call site that inlined the slot position, rather than a captured function
;; object. This is the shape the version has to reach: DEFSTRUCT redefines the
;; accessor function, but nothing rewrites the callers that already resolved it.
(deftest-compiled-only struct-redefinition.old-call-site-new-instance
  (%srd-redefined-p (lambda () (%srd-swap-inlined-a *srd-swap-new*)))
  :reported)

;; Each side is told which version it is on, so the report says whether the
;; instance or the caller is the stale one.
(deftest struct-redefinition.report-names-both-versions
  (let ((old->new (handler-case (funcall *srd-swap-old-a* *srd-swap-new*)
                    (error (e) (format nil "~a" e))))
        (new->old (handler-case (srd-swap-a *srd-swap-old*)
                    (error (e) (format nil "~a" e)))))
    (list (and (search "SRD-SWAP" old->new)
               (search "version 1" old->new)
               (search "compiled against 0" old->new)
               t)
          (and (search "version 0" new->old)
               (search "compiled against 1" new->old)
               t)))
  (t t))

;; The current definition is entirely usable -- the version is a check on stale
;; code, not a lock on the structure.
(deftest struct-redefinition.new-definition-works
  (let ((v (make-srd-swap :a 3 :b 4.5d0)))
    (setf (srd-swap-a v) 9)
    (setf (srd-swap-b v) 6.25d0)
    (list (srd-swap-a v) (srd-swap-b v)))
  (9 6.25d0))

;; COPY-STRUCTURE carries values from one instance to another, which is the one
;; operation that could hand an old instance's values to a new-version object --
;; the values would sit at the positions the old definition gave them while the
;; copy claimed to be current, and nothing would report it. The copy keeps the
;; definition its source came from.
(deftest struct-redefinition.copy-keeps-the-source-definition
  (let ((c (copy-srd-swap *srd-swap-old*)))
    (list (funcall *srd-swap-old-a* c)
          (funcall *srd-swap-old-b* c)
          (%srd-redefined-p (lambda () (srd-swap-a c)))))
  (7 1.5d0 :reported))

;;; ---- only the type changes ----

(defstruct srd-type (v 0 :type fixnum))

(defvar *srd-type-old* (make-srd-type :v 3))
(defvar *srd-type-old-v* #'srd-type-v)

(eval '(defstruct srd-type (v 0.0d0 :type double-float)))

(defvar *srd-type-new* (make-srd-type :v 1.5d0))

;; The slot did not move, but its storage changed: the raw integer became a raw
;; double. That used to surface as a failed cast from the value that came out.
;; It is the same report as a move now, from the same check.
(deftest struct-redefinition.type-change-reports-the-redefinition
  (list (%srd-redefined-p (lambda () (funcall *srd-type-old-v* *srd-type-new*)))
        (%srd-redefined-p (lambda () (srd-type-v *srd-type-old*)))
        ;; ... and each definition still works with its own instance.
        (funcall *srd-type-old-v* *srd-type-old*)
        (srd-type-v *srd-type-new*))
  (:reported :reported 3 1.5d0))

;;; ---- a FASL compiled against a later definition ----

;; The cross-image case: code compiled where the structure had already been
;; redefined, loaded where instances of the older definition are still around.
;; It must report, not read the position the slot used to have.
(deftest-compiled-only struct-redefinition.fasl-against-a-later-version
  (let ((src "srd-fasl-tmp.lisp")
        (fasl "srd-fasl-tmp.fasl"))
    (unwind-protect
        (progn
          (eval '(defstruct srd-fasl (a 0 :type fixnum) (b 0 :type fixnum)))
          (let ((old (eval '(make-srd-fasl :a 7 :b 9))))
            ;; Compiling this file resolves SRD-FASL-A to the position it has in
            ;; the definition the file carries, which is not the one that built
            ;; OLD.
            (with-open-file (s src :direction :output :if-exists :supersede)
              (format s "(defstruct srd-fasl (b 0 :type fixnum) (a 0 :type fixnum))~%")
              (format s "(defun srd-fasl-read-a (x) (srd-fasl-a x))~%"))
            (compile-file src :output-file fasl)
            (load fasl)
            ;; Reading OLD through the loaded function must not answer 9, which
            ;; is what sits at the position SRD-FASL-A now occupies.
            (%srd-redefined-p (lambda () (funcall 'srd-fasl-read-a old)))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  :reported)
