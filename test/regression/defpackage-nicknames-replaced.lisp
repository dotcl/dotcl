;;; Redefining a package with DEFPACKAGE replaces its nicknames: one the new form does not list is gone.

(defpackage :dnr-pkg (:nicknames :dnr-1 :dnr-2))
(defpackage :dnr-pkg (:nicknames :dnr-1))

(deftest defpackage-nicknames-replaced.dropped
  (list (package-nicknames :dnr-pkg) (find-package "DNR-2") (eq (find-package "DNR-1") (find-package "DNR-PKG")))
  (("DNR-1") nil t))

(defpackage :dnr-pkg)

(deftest defpackage-nicknames-replaced.all-dropped
  (list (package-nicknames :dnr-pkg) (find-package "DNR-1") (not (null (find-package "DNR-PKG"))))
  (nil nil t))

(defpackage :dnr-pkg (:nicknames :dnr-3))
(defpackage :dnr-pkg (:nicknames :dnr-3))

(deftest defpackage-nicknames-replaced.same
  (package-nicknames :dnr-pkg)
  ("DNR-3"))
