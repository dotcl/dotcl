;;; Compiled inside a unit: symbols of the unit's own PUSWANK and of the global
;;; one, resolved the first time the function runs.

(in-package "PUSWANK")

(defun lazy-syms () (list '*version* '!puswank::*version*))
