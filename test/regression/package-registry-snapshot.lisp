;;; PACKAGE-USED-BY-LIST and LIST-ALL-PACKAGES read a cached array of the
;;; distinct packages. It has to follow every change of the package registry:
;;; a new package, a deleted one, a rename, new nicknames.

(defun %prs-names (list) (sort (mapcar #'package-name list) #'string<))

(deftest package-registry-snapshot.used-by-follows-changes
  (let* ((base (make-package "PRS-BASE" :use '(:cl)))
         (r '()))
    (push (%prs-names (package-used-by-list base)) r)
    (let ((u1 (make-package "PRS-U1" :use (list base)))
          (u2 (make-package "PRS-U2" :use (list base))))
      (push (%prs-names (package-used-by-list base)) r)
      (rename-package u2 "PRS-U2-RENAMED" '("PRS-U2-NICK"))
      (push (%prs-names (package-used-by-list base)) r)
      (push (and (find-package "PRS-U2-NICK") t) r)
      (delete-package u1)
      (push (%prs-names (package-used-by-list base)) r)
      (unuse-package base u2)
      (push (%prs-names (package-used-by-list base)) r)
      (delete-package u2))
    (delete-package base)
    (nreverse r))
  (() ("PRS-U1" "PRS-U2") ("PRS-U1" "PRS-U2-RENAMED") t ("PRS-U2-RENAMED") ()))

(deftest package-registry-snapshot.list-all-packages
  (let* ((before (length (list-all-packages)))
         (p (make-package "PRS-ALL"))
         (during (length (list-all-packages))))
    (delete-package p)
    (list (- during before) (- (length (list-all-packages)) before)
          (and (member "PRS-ALL" (mapcar #'package-name (list-all-packages)) :test #'string=) t)))
  (1 0 nil))
