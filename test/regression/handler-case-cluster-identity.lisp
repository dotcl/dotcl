;;; Entering a HANDLER-CASE allocates only its cluster.
;;;
;;; A handler-case used to allocate, on every entry and whether or not anything was
;;; signalled: a tag object to identify the invocation, an array of the clause type
;;; specifiers for the filter, and the cluster of bindings. The first two said what
;;; the third already said -- the cluster array is allocated once per entry and is
;;; therefore already unique, and it carries the specifiers.
;;;
;;; So the invocation is now identified by the cluster array itself. The tests that
;;; matter are the ones where identity decides: nested handler-cases, and the same
;;; handler-case entered twice at once.

(define-condition hci-a (error) ())
(define-condition hci-b (hci-a) ())

;;; Ordinary dispatch: by type, in clause order, most specific clause wins by being
;;; written first.
(deftest handler-case-identity.by-type
  (list (handler-case (error 'hci-b) (hci-b () :b) (hci-a () :a))
        (handler-case (error 'hci-a) (hci-b () :b) (hci-a () :a))
        (handler-case :none (hci-a () :a)))
  (:b :a :none))

;;; The body's values are the handler-case's, including several of them.
(deftest handler-case-identity.body-values
  (multiple-value-list (handler-case (values 1 2 3) (hci-a () :a)))
  (1 2 3))

;;; Nested: the inner one must take it, and the outer must be left alone. If the two
;;; invocations could not be told apart, the condition would surface at whichever
;;; frame the unwind reached first.
(deftest handler-case-identity.nested-inner-wins
  (handler-case
      (list :outer (handler-case (error 'hci-b) (hci-b () :inner)))
    (hci-a () :outer-caught))
  (:outer :inner))

;;; Nested where only the OUTER can handle it: the inner one must decline and the
;;; condition must reach the outer clause.
(deftest handler-case-identity.nested-outer-catches
  (handler-case
      (handler-case (error 'hci-a) (hci-b () :inner))
    (hci-a () :outer))
  :outer)

;;; The same handler-case entered twice at once. Both activations exist on the
;;; cluster stack simultaneously, and each has to catch its own -- this is what the
;;; per-entry identity is for.
(defun %hci-rec (depth)
  (handler-case
      (if (plusp depth)
          (list depth (%hci-rec (1- depth)))
          (error 'hci-a))
    (hci-a () (list :caught-at depth))))

(deftest handler-case-identity.same-form-nested
  (%hci-rec 2)
  (2 (1 (:caught-at 0))))

;;; HANDLER-BIND uses the other binding shape (a handler function, not a clause) and
;;; must keep working alongside.
(deftest handler-case-identity.handler-bind-still-works
  (let ((seen nil))
    (list (handler-case
              (handler-bind ((hci-a (lambda (c) (declare (ignore c)) (push :bind seen))))
                (error 'hci-b))
            (hci-a () :case))
          seen))
  (:case (:bind)))

;;; A raw .NET exception reaches the type-matching half of the filter, which now
;;; reads the specifiers out of the cluster rather than a separate array.
(deftest handler-case-identity.raw-dotnet-error
  (handler-case (dotnet:invoke "System.Int32" "Parse" "not-a-number")
    (error () :caught))
  :caught)

;;; A clause that does not match must let the condition through.
(deftest handler-case-identity.no-clause-matches
  (handler-case
      (handler-case (error 'hci-a) (type-error () :wrong))
    (hci-a () :right))
  :right)

;;; The point of the change. Entering a handler-case that signals nothing should
;;; cost only its cluster: the tag object and the duplicate specifier array were
;;; 56 bytes on top of it.
(defvar *hci-sink* nil)

(defun %hci-enter (n)
  (declare (fixnum n))
  (do ((i 0 (1+ i))) ((= i n) *hci-sink*)
    (declare (fixnum i))
    (setq *hci-sink* (handler-case i (hci-a () 0)))))

(deftest-compiled-only handler-case-identity.entry-allocation
  (< (bytes-per-op #'%hci-enter) 100)
  t)
