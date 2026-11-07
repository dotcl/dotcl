;;; DO-SYMBOLS and DO-EXTERNAL-SYMBOLS visit the same symbols as before on a random graph of packages
;;; (use, shadow, export, unexport). The expected sets were produced by the build that walked the
;;; use list one FIND-SYMBOL at a time.

(defun %dsg-build ()
  (let ((st 4242) (pk nil))
    (flet ((rnd (n) (setq st (mod (+ (* st 1103515245) 12345) 2147483648)) (mod (ash st -8) n)))
      (dotimes (i 8)
        (let ((name (format nil "DSG~d" i)))
          (when (find-package name) (delete-package name))))
      (dotimes (i 8) (push (make-package (format nil "DSG~d" i) :use nil) pk))
      (setq pk (nreverse pk))
      (dotimes (step 250)
        (let ((p (nth (rnd 8) pk)) (q (nth (rnd 8) pk)))
          (ignore-errors
           (case (rnd 6)
             (0 (intern (format nil "N~d" (rnd 40)) p))
             (1 (export (intern (format nil "N~d" (rnd 40)) p) p))
             (2 (unless (eq p q) (use-package q p)))
             (3 (shadow (format nil "N~d" (rnd 40)) p))
             (4 (unexport (intern (format nil "N~d" (rnd 40)) p) p))
             (5 (unless (eq p q) (unuse-package q p)))))))
      pk)))

(deftest do-symbols-graph.sets
  (let ((r nil))
    (dolist (p (%dsg-build))
      (let ((all nil) (ext nil))
        (do-symbols (s p)
          (pushnew (format nil "~a:~a" (package-name (symbol-package s)) (symbol-name s)) all :test #'string=))
        (do-external-symbols (s p) (push (symbol-name s) ext))
        (push (list (package-name p) (sort all #'string<) (sort (copy-list ext) #'string<)) r)))
    (nreverse r))
 (("DSG0" ("DSG0:N0" "DSG0:N1" "DSG0:N12" "DSG0:N14" "DSG0:N16" "DSG0:N2" "DSG0:N22" "DSG0:N23" "DSG0:N24" "DSG0:N27" "DSG0:N30" "DSG0:N32" "DSG0:N33" "DSG0:N37" "DSG0:N38" "DSG0:N6" "DSG0:N7" "DSG0:N8") ("N16" "N23" "N7")) ("DSG1" ("DSG1:N1" "DSG1:N10" "DSG1:N15" "DSG1:N16" "DSG1:N17" "DSG1:N2" "DSG1:N23" "DSG1:N28" "DSG1:N3" "DSG1:N31" "DSG1:N35" "DSG1:N5" "DSG1:N7" "DSG1:N8" "DSG2:N33" "DSG6:N19" "DSG6:N22") ("N10" "N17" "N23" "N3" "N5" "N7")) ("DSG2" ("DSG2:N11" "DSG2:N12" "DSG2:N15" "DSG2:N16" "DSG2:N18" "DSG2:N19" "DSG2:N2" "DSG2:N20" "DSG2:N23" "DSG2:N24" "DSG2:N27" "DSG2:N29" "DSG2:N3" "DSG2:N30" "DSG2:N31" "DSG2:N33" "DSG2:N35" "DSG2:N37" "DSG2:N38" "DSG2:N4" "DSG2:N6") ("N3" "N33" "N35")) ("DSG3" ("DSG2:N3" "DSG2:N33" "DSG2:N35" "DSG3:N11" "DSG3:N12" "DSG3:N19" "DSG3:N2" "DSG3:N21" "DSG3:N22" "DSG3:N27" "DSG3:N28" "DSG3:N30" "DSG3:N31" "DSG3:N32" "DSG3:N34" "DSG3:N39" "DSG3:N7" "DSG4:N38" "DSG7:N14" "DSG7:N29" "DSG7:N36" "DSG7:N4" "DSG7:N9") ("N2" "N30" "N32" "N34")) ("DSG4" ("DSG1:N10" "DSG1:N17" "DSG1:N23" "DSG1:N3" "DSG1:N5" "DSG1:N7" "DSG4:N0" "DSG4:N13" "DSG4:N15" "DSG4:N20" "DSG4:N24" "DSG4:N29" "DSG4:N35" "DSG4:N38" "DSG4:N39" "DSG4:N6" "DSG4:N9" "DSG5:N30" "DSG6:N1" "DSG6:N19" "DSG6:N22") ("N38")) ("DSG5" ("DSG1:N10" "DSG1:N17" "DSG1:N23" "DSG1:N3" "DSG1:N5" "DSG1:N7" "DSG5:N0" "DSG5:N1" "DSG5:N11" "DSG5:N14" "DSG5:N15" "DSG5:N16" "DSG5:N18" "DSG5:N20" "DSG5:N30" "DSG5:N31" "DSG5:N32" "DSG5:N33" "DSG5:N34" "DSG5:N37" "DSG5:N8") ("N23" "N30")) ("DSG6" ("DSG6:N1" "DSG6:N14" "DSG6:N19" "DSG6:N22" "DSG6:N24" "DSG6:N26" "DSG6:N27" "DSG6:N29" "DSG6:N31" "DSG6:N32" "DSG6:N35" "DSG6:N4" "DSG6:N5" "DSG6:N6" "DSG6:N9") ("N1" "N19" "N22")) ("DSG7" ("DSG1:N10" "DSG1:N17" "DSG1:N23" "DSG1:N3" "DSG1:N5" "DSG1:N7" "DSG5:N30" "DSG7:N11" "DSG7:N14" "DSG7:N2" "DSG7:N20" "DSG7:N24" "DSG7:N26" "DSG7:N27" "DSG7:N29" "DSG7:N31" "DSG7:N34" "DSG7:N36" "DSG7:N38" "DSG7:N4" "DSG7:N8" "DSG7:N9") ("N14" "N29" "N36" "N4" "N9"))))

(deftest do-symbols-graph.counts-cl
  (let ((n 0) (m 0))
    (do-external-symbols (s :cl) (incf n))
    (do-symbols (s :cl) (incf m))
    (and (<= n m) (> n 900)))
  t)
