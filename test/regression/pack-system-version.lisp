;;; `dotcl pack` reads the nuspec version off the system definition.
;;;
;;; The other nuspec defaults (:description, :homepage, :source-control,
;;; :author, :license, and a README next to the .asd) were already read from the
;;; system; :version alone was not, so every project that wanted one source of
;;; truth for its version had to dig it out of the .asd in its build script
;;; before calling pack. What is pinned here is the reading step: the version
;;; reaches DotclBuild.ReadSystemMeta's SystemMeta, and a .asd that states no
;;; version leaves it NIL while the other fields still come through, which is
;;; what makes the command line's --version required again in exactly that case.
;;;
;;; The command-line half -- --version defaulting to this value, an explicit
;;; --version winning over it, and the usage error when neither exists -- is
;;; asserted end to end in test/pack-nuspec/check.sh, which produces real
;;; packages and can look at the version in the nuspec. It needs a donor set of
;;; published dotcl packages, which is why it is not here.
;;;
;;; NOTE: stay in the default load package (CL-USER); an (in-package ...) into a
;;; (:use :cl)-only package hides the framework's DEFTEST macro. Helpers are
;;; psv- prefixed. The fixture systems live in .asd files written to a temp dir
;;; rather than in this file, because a DEFSYSTEM inside a file the suite LOADs
;;; makes ASDF treat this file as the system's definition and read it again.

(require "asdf")

(defvar *psv-dir* "test/regression/.tmp-pack-version/")

(defun psv-write (name text)
  (with-open-file (s (merge-pathnames name (truename *psv-dir*))
                     :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun psv-setup ()
  "Write the fixture systems and make ASDF able to find them.
One .asd per system: ASDF warns about a system whose name does not match the
file it was found in, and the warning is noise the suite does not need."
  (ensure-directories-exist *psv-dir*)
  (psv-write "psv-versioned.asd"
             "(defsystem \"psv-versioned\"
  :version \"0.9.2\"
  :description \"Fixture that states its version\"
  :author \"Fixture Author\"
  :license \"MIT\"
  :components ())
(defsystem \"psv-versioned/exe\"
  :description \"Secondary system that states no version\"
  :components ())
(defsystem \"psv-versioned/pinned\"
  :version \"2.0.0\"
  :components ())")
  (psv-write "psv-unversioned.asd"
             "(defsystem \"psv-unversioned\"
  :description \"Fixture that states no version\"
  :author \"Fixture Author\"
  :license \"MIT\"
  :components ())")
  (pushnew (pathname (namestring (truename *psv-dir*)))
           asdf:*central-registry* :test #'equal))

(defun psv-meta (system)
  "Read SYSTEM's nuspec metadata the way `dotcl pack` does."
  (psv-setup)
  (dotnet:static "DotCL.DotclBuild" "ReadSystemMeta" system nil))

;;; The whole point: a system that states :version needs no --version.
(deftest pack-system-version.version-is-read-from-the-asd
  (dotnet:invoke (psv-meta "psv-versioned") "Version")
  "0.9.2")

;;; The version travels with the fields that were already being read, rather
;;; than through some second path of its own.
(deftest pack-system-version.version-comes-with-the-other-fields
  (let ((m (psv-meta "psv-versioned")))
    (list (dotnet:invoke m "Version")
          (dotnet:invoke m "Description")
          (dotnet:invoke m "Author")
          (dotnet:invoke m "License")))
  ("0.9.2" "Fixture that states its version" "Fixture Author" "MIT"))

;;; A silent .asd leaves the version unset -- NIL, not "" or "0.0.0" -- so pack
;;; still demands --version instead of inventing a version nobody wrote. The
;;; same NIL is what a version ASDF hands back as a non-string would produce.
(deftest pack-system-version.no-version-in-the-asd-stays-nil
  (dotnet:invoke (psv-meta "psv-unversioned") "Version")
  nil)

;;; ... while everything else about that system is still read, so the missing
;;; version is the only thing the command line has to supply.
(deftest pack-system-version.other-fields-survive-a-missing-version
  (let ((m (psv-meta "psv-unversioned")))
    (list (dotnet:invoke m "Version")
          (dotnet:invoke m "Description")
          (dotnet:invoke m "Author")
          (dotnet:invoke m "License")))
  (nil "Fixture that states no version" "Fixture Author" "MIT"))

;;; A secondary system (primary/name) that states no :version takes its primary
;;; system's, as its author and license already did; one of its own wins.
(deftest pack-system-version.secondary-system-falls-back-to-primary
  (list (dotnet:invoke (psv-meta "psv-versioned/exe") "Version")
        (dotnet:invoke (psv-meta "psv-versioned/exe") "Author")
        (dotnet:invoke (psv-meta "psv-versioned/pinned") "Version"))
  ("0.9.2" "Fixture Author" "2.0.0"))
