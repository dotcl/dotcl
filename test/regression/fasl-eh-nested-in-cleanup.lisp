;;; An exception block nested inside an unwind-protect cleanup must survive the
;;; FASL round trip.
;;;
;;; PersistedAssemblyBuilder orders a method's exception clauses by where their
;;; try blocks end. That is correct while nesting happens through try blocks, but
;;; a try nested inside an enclosing clause's HANDLER starts after that handler's
;;; try has already ended, so the enclosing clause is written first — and the CLR
;;; rejects the method with InvalidProgramException as soon as it is JITted.
;;; dotcl reorders the clauses after saving the fasl.
;;;
;;; The same code compiled in memory was always fine (the runtime ILGenerator
;;; orders the clauses correctly), so these tests must go through compile-file
;;; and load to mean anything. The shapes below are the ones real code hits:
;;; ignore-errors and catch in an unwind-protect cleanup, which is how ASDF
;;; deletes temporary files and searches the central registry.

(deftest-compiled-only fasl-eh-nested-in-cleanup
  (let* ((tmp (uiop:temporary-directory))
         (src (merge-pathnames "rf-ehorder.lisp" tmp))
         (out (merge-pathnames "rf-ehorder.fasl" tmp)))
    (with-open-file (f src :direction :output :if-exists :supersede)
      (write-line "(defun rf-eh-catch (x) (unwind-protect x (catch 'tag (throw 'tag 1))))" f)
      (write-line "(defun rf-eh-ignore (x) (unwind-protect x (ignore-errors (error \"e\"))))" f)
      (write-line "(defun rf-eh-restart (x)" f)
      (write-line "  (unwind-protect x" f)
      (write-line "    (handler-bind ((error (lambda (c) (declare (ignore c))" f)
      (write-line "                            (invoke-restart 'r))))" f)
      (write-line "      (restart-case (error \"e\") (r () nil)))))" f)
      ;; The cleanup runs on the non-local exit too, not just on normal return.
      (write-line "(defun rf-eh-thrown (x)" f)
      (write-line "  (catch 'outer" f)
      (write-line "    (unwind-protect (throw 'outer x)" f)
      (write-line "      (ignore-errors (error \"e\")))))" f))
    (compile-file src :output-file out)
    (load out)
    (list (funcall (find-symbol "RF-EH-CATCH") 1)
          (funcall (find-symbol "RF-EH-IGNORE") 2)
          (funcall (find-symbol "RF-EH-RESTART") 3)
          (funcall (find-symbol "RF-EH-THROWN") 4)))
  (1 2 3 4))
