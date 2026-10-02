;;; `dotcl pack` warns when the version it took from the .asd is one NuGet
;;; cannot serve.
;;;
;;; ASDF lets "1.2.3.4.5" through as a :version, pack stamps the nupkg with it,
;;; and `dotnet tool install` then answers only that the package is not found --
;;; nothing points at the version. Pack now says so while it still has the
;;; version in hand, but still writes the package (it is a warning, not a
;;; refusal), and only for a version it defaulted from the .asd. What is pinned
;;; here is the rule the warning uses; the command-line half runs in
;;; test/pack-nuspec/check.sh, which needs published dotcl packages as donors.

(defun pavn-problem (version)
  (dotnet:static "DotCL.DotclBuild" "AsdVersionNuGetProblem" version))

;;; One to four integer components: what NuGet serves.
(deftest pack-asd-version-nuget.accepted
  (mapcar #'pavn-problem '("1" "0.9" "0.9.2" "1.2.3.4" "2147483647.0"))
  (nil nil nil nil nil))

;;; Five or more components is the shape that reaches pack in practice.
(deftest pack-asd-version-nuget.five-components
  (let ((p (pavn-problem "1.2.3.4.5")))
    (and (stringp p) (search "5 components" p) t))
  t)

;;; A component beyond Int32 is refused by NuGet as well.
(deftest pack-asd-version-nuget.component-too-large
  (let ((p (pavn-problem "2147483648.0")))
    (and (stringp p) (search "2147483648" p) t))
  t)
