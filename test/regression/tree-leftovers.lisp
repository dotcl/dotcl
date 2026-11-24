;;; The run must not leave temporary files in the working directory: tests that
;;; compile or write files use REGRESSION-TEMP-FILE (see test/framework.lisp).
;;; Loaded last, so it sees everything the run created.

(deftest tree-leftovers.none-left-in-the-working-directory
  (set-difference (tree-leftovers) *tree-leftovers-before* :test #'string=)
  nil)
