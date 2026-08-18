;;; An interpreted closure must report the same arity as a compiled one.
;;;
;;; The tree-walk evaluator builds every closure as one variadic
;;; (lambda (&rest call-args) ...), keeping the user's lambda list as data for the
;;; binder. Arity counts REQUIRED parameters, so every interpreted function used to
;;; claim zero of them. Overload selection for a .NET delegate parameter reads it:
;;; Enumerable.Select has a Func<T,R> and a Func<T,int,R> overload told apart by
;;; arity, so a one-argument Lisp lambda matched NEITHER and the call failed with
;;; "Method ... Select not found" — on any build when interpreted, and on an
;;; emit-free build always, since there everything is interpreted.
;;;
;;; The arity is asserted through its consequence rather than read directly:
;;; whether a candidate overload is found at all.

(defun %ica-ints ()
  (let ((l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                 (list "System.Int32")))))
    (dolist (x '(1 2 3) l) (dotnet:invoke l "Add" x))))

(defun %ica-resolves (seq) (not (null seq)))

(deftest interp-closure-arity.compiled-lambda-selects-linq-overload
  (%ica-resolves (dotnet:invoke (%ica-ints) "Select" (lambda (x) (* x 10))))
  t)

(deftest interp-closure-arity.interpreted-lambda-selects-linq-overload
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(%ica-resolves
            (dotnet:invoke (%ica-ints) "Select" (lambda (x) (* x 10))))))
  t)

;;; The two-argument overload is still reachable: a two-parameter Lisp lambda must
;;; select Func<T,int,R> (element + index), interpreted as well as compiled.

(deftest interp-closure-arity.interpreted-two-arg-lambda-selects-indexed-overload
  (let ((dotcl:*evaluator-mode* :interpret))
    (eval '(%ica-resolves
            (dotnet:invoke (%ica-ints) "Select" (lambda (x i) (+ (* x 10) i))))))
  t)
