;;; Reading and SETQ of a special variable that has never been bound take the
;;; global value directly; once bound anywhere, the binding is seen.

(defvar *sgfp* 0)
(defvar *sgfp-unbound*)
(defun %sgfp-bump () (setq *sgfp* (1+ *sgfp*)) *sgfp*)
(defun %sgfp-read-unbound () *sgfp-unbound*)
(defun %sgfp-set-unbound (v) (setq *sgfp-unbound* v))

(deftest special-global-fast-path.read-setq-bind
  (list (progn (dotimes (i 100) (%sgfp-bump)) *sgfp*)
        (let ((*sgfp* 1000)) (list (%sgfp-bump) (%sgfp-bump)))
        *sgfp*
        (%sgfp-bump))
  (100 (1001 1002) 100 101))

(deftest special-global-fast-path.unbound
  (list (handler-case (%sgfp-read-unbound) (unbound-variable () :unbound))
        (progn (%sgfp-set-unbound 5) (%sgfp-read-unbound))
        (progn (makunbound '*sgfp-unbound*)
               (handler-case (%sgfp-read-unbound) (unbound-variable () :unbound))))
  (:unbound 5 :unbound))
