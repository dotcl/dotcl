;;; A DEFVAR compiled on several threads at once: every variable ends up known
;;; to the compiler as special. COMPILE-DEFVAR pushed onto the compiler's lists
;;; of special variables without a lock, so a push could overwrite another
;;; thread's, and a later LET of the lost variable was compiled as a lexical
;;; binding.

(require "dotcl-thread")

(deftest global-specials-threads.none-lost
  (let* ((nthreads 4) (per 3000) (go nil)
         (syms (loop repeat nthreads
                     collect (loop repeat per collect (make-symbol "GST-VAR"))))
         (threads (loop for mine in syms
                        collect (let ((mine mine))
                                  (dotcl-thread:make-thread
                                   (lambda ()
                                     (loop until go do (dotcl-thread:thread-yield))
                                     (dolist (s mine)
                                       (dotcl.cil-compiler::compile-defvar s 0 t))))))))
    (setf go t)
    (mapc #'dotcl-thread:thread-join threads)
    (unwind-protect
         (loop for mine in syms
               sum (count-if-not #'dotcl.cil-compiler::global-special-p mine) into global
               sum (count-if-not #'dotcl.cil-compiler::special-var-p mine) into any
               finally (return (list global any)))
      ;; Don't leave 12000 entries in the compiler for the rest of the suite.
      (let ((ours (make-hash-table :test 'eq)))
        (dolist (mine syms)
          (dolist (s mine)
            (setf (gethash s ours) t)
            (remhash s dotcl-internal::*global-specials*)))
        (setf dotcl-internal::*specials*
              (remove-if (lambda (s) (gethash s ours)) dotcl-internal::*specials*)))))
  (0 0))

(deftest global-specials-threads.cleaned-up
  (list (count "GST-VAR" dotcl-internal::*specials* :key #'symbol-name :test #'string=)
        (loop for s being the hash-keys of dotcl-internal::*global-specials*
              count (and (symbolp s) (string= (symbol-name s) "GST-VAR"))))
  (0 0))
