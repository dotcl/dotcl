;;; Compiling a function that calls itself (not in tail position) rewrites its
;;; instruction list, and that rewrite used to recurse once per instruction.
;;; A long enough function overflowed the stack of a thread other than the
;;; main one (whose stack is much larger). SLY compiles its contribs in such a
;;; thread (slynk-arglists' PRINT-DECODED-ARGLIST, a &key function) and
;;; dropped the connection when it happened. The rewrite is called directly
;;; here on a long instruction list, in a thread.

(require "dotcl-thread")

(deftest sil-subst-self-arg0-long-list-in-thread
  (let ((result nil))
    (dotcl-thread:thread-join
     (dotcl-thread:make-thread
      (lambda ()
        (setq result
              (handler-case
                  (let* ((key (make-symbol "SELF"))
                         (instrs (append (make-list 400000 :initial-element '(:nop))
                                         (list (list :ldloc key) '(:ret))))
                         (out (dotcl.cil-compiler::%sil-subst-self-arg0 instrs key)))
                    (list (length out) (car (last out 2)) (eq (car out) (car instrs))))
                (serious-condition (e) (princ-to-string e)))))))
    result)
  (400002 (:ldarg 0) t))

(deftest sil-subst-self-arg0-sharing
  ;; An unchanged list comes back as the same object; a changed one shares
  ;; the unchanged tail after the last change.
  (let* ((key (make-symbol "SELF"))
         (tail (list '(:ret)))
         (same (list* '(:nop) '(:nop) tail))
         (changed (list* '(:nop) (list :ldloc key) tail))
         (out (dotcl.cil-compiler::%sil-subst-self-arg0 changed key)))
    (list (eq (dotcl.cil-compiler::%sil-subst-self-arg0 same key) same)
          out
          (eq (cddr out) tail)
          (dotcl.cil-compiler::%sil-subst-self-arg0 (list :ldloc key) key)
          (dotcl.cil-compiler::%sil-subst-self-arg0 (list '(:load-const (:ldloc x)) (list :ldloc key)) key)))
  (t ((:nop) (:ldarg 0) (:ret)) t (:ldarg 0) ((:load-const (:ldloc x)) (:ldarg 0))))
