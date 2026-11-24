;;; DOTCL:GC accepts the SBCL-style keywords :FULL and :GEN. (GC) still runs a
;;; full collection with the pending finalizers; :FULL T does the same, :FULL
;;; NIL and :GEN N collect only up to the requested generation. An unknown
;;; keyword is still a program error.

;;; The target is only referenced by the weak pointer once this returns.
(defun %gckw-make-weak-pointer ()
  (dotcl:make-weak-pointer (list :gckw-unreferenced)))

(deftest gc-keyword-args.no-args
  (dotcl:gc)
  nil)

(deftest gc-keyword-args.full-t
  (dotcl:gc :full t)
  nil)

(deftest gc-keyword-args.full-nil
  (dotcl:gc :full nil)
  nil)

(deftest gc-keyword-args.gen-minimum
  (dotcl:gc :gen 0)
  nil)

(deftest gc-keyword-args.gen-maximum
  (dotcl:gc :gen 2)
  nil)

(deftest gc-keyword-args.full-clears-weak-pointer
  (let ((wp (%gckw-make-weak-pointer)))
    (dotcl:gc :full t)
    (dotcl:weak-pointer-value wp))
  nil)

(deftest gc-keyword-args.unknown-keyword
  (signals-error (dotcl:gc :bogus 1) error)
  t)

(deftest gc-keyword-args.odd-keywords
  (signals-error (dotcl:gc :full) error)
  t)
