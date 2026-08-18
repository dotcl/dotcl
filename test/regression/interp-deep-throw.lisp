;;; A non-local exit out of deep interpreted recursion must not kill the process.
;;;
;;; The tree-walk evaluator wraps every interpreted call in a BLOCK, which it runs
;;; as a CATCH. While CATCH matched its tag by catching every CatchThrowException
;;; and rethrowing the ones belonging to an outer CATCH, every frame a THROW
;;; crossed left a live handler funclet and a restarted exception dispatch behind
;;; it, so a THROW cost stack proportional to the depth it travelled — about as
;;; much again as the recursion had already used. Deep enough recursion therefore
;;; turned a catchable STORAGE-CONDITION into a fatal .NET StackOverflowException:
;;; the handler ran, and the RETURN-FROM leaving it died on the way out, taking
;;; the process with it. Matching the tag in a CIL exception FILTER instead lets a
;;; frame decline without the throw ever unwinding into it.

(defun %idt (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (eval form)))

;;; A THROW that crosses thousands of interpreted frames still lands on its own
;;; CATCH and carries its value.

(deftest interp-deep-throw.crosses-deep-recursion
  (%idt :interpret
        '(progn
          (defun %idt-rec (n) (if (zerop n) (throw :deep :thrown) (1+ (%idt-rec (- n 1)))))
          (catch :deep (%idt-rec 2000))))
  :thrown)

;;; RETURN-FROM out of the same depth: the shape a HANDLER-CASE clause uses.

(deftest interp-deep-throw.return-from-deep-recursion
  (%idt :interpret
        '(block %idt-outer
          (labels ((rec (n) (if (zerop n) (return-from %idt-outer :returned) (1+ (rec (- n 1))))))
            (rec 2000))))
  :returned)

;;; The runaway case: interpreted recursion that exhausts the control stack must
;;; arrive as a catchable STORAGE-CONDITION rather than killing the process.

(deftest interp-deep-throw.storage-condition-catchable
  (%idt :interpret
        '(progn
          (defun %idt-runaway (n) (if (zerop n) 0 (+ 1 (%idt-runaway (- n 1)))))
          (handler-case (%idt-runaway 10000000)
            (storage-condition () :caught-storage-condition))))
  :caught-storage-condition)
