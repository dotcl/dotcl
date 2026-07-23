;;; driver.lisp — compile corpus.lisp (honoring DOTCL_EMIT_PDB), load the fasl,
;;; and print the canonical results. Run once with DOTCL_EMIT_PDB unset and once
;;; with it set; the harness diffs the two outputs. The corpus path comes from
;;; the CORPUS_SRC env var so the same driver serves both runs.
(in-package :cl-user)

(let* ((src (or (dotcl:getenv "CORPUS_SRC") "corpus.lisp"))
       (fasl (compile-file src)))
  (load fasl)
  ;; Sentinels let the harness extract just the results, ignoring any build /
  ;; loader noise on stdout.
  (format t "~&===CORPUS-BEGIN===~%")
  (funcall (intern "RUN-CORPUS" :cl-user))
  (format t "===CORPUS-END===~%"))
