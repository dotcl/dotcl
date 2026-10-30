;;; A released seed already has these macros; the probe redefines them and uses them.
(in-package :dotcl.cil-compiler)

(defmacro do-instr-local-reads ((var instr) &body body)
  (declare (ignore var instr body))
  '(list :tree-definition-wins))

(defun xc-macro-probe-use (instr)
  (declare (ignore instr))
  (do-instr-local-reads (k instr) nil))

(defmacro do-instr-local-writes ((var instr) &body body)
  (declare (ignore var instr body))
  '(list :tree-definition-wins))

(defun xc-macro-probe-write (instr)
  (declare (ignore instr))
  (do-instr-local-writes (k instr) nil))
