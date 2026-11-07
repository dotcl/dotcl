;;; The body of DO-SYMBOLS / DO-EXTERNAL-SYMBOLS may unintern, export, intern, shadow, import,
;;; unexport or unuse while the iteration runs: it finishes without error and the package ends
;;; up as it did when the tables were copied before the walk. Expected values come from the build
;;; that copied them.

(defun %dsm-fresh (name n)
  (when (find-package name) (delete-package name))
  (let ((p (make-package name :use '(:cl))))
    (dotimes (i n) (let ((s (intern (format nil "S~d" i) p))) (when (evenp i) (export s p))))
    p))
(defun %dsm-own (p) (let (r) (do-symbols (s p) (when (eq (symbol-package s) p) (push (symbol-name s) r))) (sort r #'string<)))
(defun %dsm-eown (p) (let (r) (do-external-symbols (s p) (push (symbol-name s) r)) (sort r #'string<)))

(deftest do-symbols-mutation.unintern-all
  (let ((p (%dsm-fresh "DSM1" 40)) (c 0)) (do-symbols (s p) (incf c) (when (eq (symbol-package s) p) (unintern s p))) (list c (%dsm-own p)))
  (1018 NIL))

(deftest do-symbols-mutation.unintern-even
  (let ((p (%dsm-fresh "DSM2" 40)) (c 0)) (do-symbols (s p) (incf c) (when (and (eq (symbol-package s) p) (evenp (length (symbol-name s)))) (unintern s p))) (list c (%dsm-own p)))
  (1018 ("S10" "S11" "S12" "S13" "S14" "S15" "S16" "S17" "S18" "S19" "S20" "S21" "S22" "S23" "S24" "S25" "S26" "S27" "S28" "S29" "S30" "S31" "S32" "S33" "S34" "S35" "S36" "S37" "S38" "S39")))

(deftest do-symbols-mutation.export-intern
  (let ((p (%dsm-fresh "DSM3" 40)) (c 0)) (do-symbols (s p) (incf c) (when (eq (symbol-package s) p) (export s p) (intern (format nil "X~a" (symbol-name s)) p))) (list c (%dsm-own p)))
  (1018 ("S0" "S1" "S10" "S11" "S12" "S13" "S14" "S15" "S16" "S17" "S18" "S19" "S2" "S20" "S21" "S22" "S23" "S24" "S25" "S26" "S27" "S28" "S29" "S3" "S30" "S31" "S32" "S33" "S34" "S35" "S36" "S37" "S38" "S39" "S4" "S5" "S6" "S7" "S8" "S9" "XS0" "XS1" "XS10" "XS11" "XS12" "XS13" "XS14" "XS15" "XS16" "XS17" "XS18" "XS19" "XS2" "XS20" "XS21" "XS22" "XS23" "XS24" "XS25" "XS26" "XS27" "XS28" "XS29" "XS3" "XS30" "XS31" "XS32" "XS33" "XS34" "XS35" "XS36" "XS37" "XS38" "XS39" "XS4" "XS5" "XS6" "XS7" "XS8" "XS9")))

(deftest do-symbols-mutation.shadow
  (let ((p (%dsm-fresh "DSM4" 40)) (c 0)) (do-symbols (s p) (incf c) (when (eq (symbol-package s) p) (shadow (symbol-name s) p))) (list c (%dsm-own p)))
  (1018 ("S0" "S1" "S10" "S11" "S12" "S13" "S14" "S15" "S16" "S17" "S18" "S19" "S2" "S20" "S21" "S22" "S23" "S24" "S25" "S26" "S27" "S28" "S29" "S3" "S30" "S31" "S32" "S33" "S34" "S35" "S36" "S37" "S38" "S39" "S4" "S5" "S6" "S7" "S8" "S9")))

(deftest do-symbols-mutation.import
  (let ((p (%dsm-fresh "DSM5" 40)) (q (%dsm-fresh "DSM5Q" 5)) (c 0)) (do-symbols (s p) (incf c) (when (< c 10) (import (intern "ZZ" q) p))) (list c (%dsm-own p)))
  (1018 ("S0" "S1" "S10" "S11" "S12" "S13" "S14" "S15" "S16" "S17" "S18" "S19" "S2" "S20" "S21" "S22" "S23" "S24" "S25" "S26" "S27" "S28" "S29" "S3" "S30" "S31" "S32" "S33" "S34" "S35" "S36" "S37" "S38" "S39" "S4" "S5" "S6" "S7" "S8" "S9")))

(deftest do-symbols-mutation.unexport
  (let ((p (%dsm-fresh "DSM6" 40)) (c 0)) (do-external-symbols (s p) (incf c) (unexport s p)) (list c (%dsm-eown p)))
  (20 NIL))

(deftest do-symbols-mutation.ext-add
  (let ((p (%dsm-fresh "DSM7" 40)) (c 0)) (do-external-symbols (s p) (incf c) (intern (format nil "Y~d" c) p) (export (intern (format nil "W~d" c) p) p)) (list c (%dsm-eown p)))
  (20 ("S0" "S10" "S12" "S14" "S16" "S18" "S2" "S20" "S22" "S24" "S26" "S28" "S30" "S32" "S34" "S36" "S38" "S4" "S6" "S8" "W1" "W10" "W11" "W12" "W13" "W14" "W15" "W16" "W17" "W18" "W19" "W2" "W20" "W3" "W4" "W5" "W6" "W7" "W8" "W9")))

(deftest do-symbols-mutation.unuse
  (let ((p (%dsm-fresh "DSM8" 40)) (u (progn (when (find-package "DSM8U") (delete-package "DSM8U")) (make-package "DSM8U" :use nil))) (c 0)) (dotimes (i 5) (export (intern (format nil "U~d" i) u) u)) (use-package u p) (do-symbols (s p) (incf c) (when (< c 20) (unuse-package u p))) (list c (%dsm-own p)))
  (1023 ("S0" "S1" "S10" "S11" "S12" "S13" "S14" "S15" "S16" "S17" "S18" "S19" "S2" "S20" "S21" "S22" "S23" "S24" "S25" "S26" "S27" "S28" "S29" "S3" "S30" "S31" "S32" "S33" "S34" "S35" "S36" "S37" "S38" "S39" "S4" "S5" "S6" "S7" "S8" "S9")))

