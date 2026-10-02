;;; A fasl that is itself a ReadyToRun image (a per-RID package ships
;;; asdf.fasl that way, under its own name) is loaded by path, so that its
;;; native code is used. Read into a byte array its code was ignored and every
;;; top level form was JIT-compiled at each load. The check reads only the PE
;;; headers.

(defun %r2rp (path)
  (dotnet:static "DotCL.Runtime" "IsReadyToRunImage" (namestring path)))

(deftest fasl-r2r-in-place.detects
  (list
   ;; The framework's own assemblies are ReadyToRun images.
   (%r2rp (dotnet:invoke (dotnet:invoke (dotnet:static "System.Type" "GetType" "System.Object")
                                        "Assembly")
                         "Location"))
   ;; An IL-only image, a file that is not PE at all, and one that does not exist.
   (%r2rp (truename "compiler/dotcl.core"))
   (%r2rp (truename "test/regression/fasl-r2r-in-place.lisp"))
   (%r2rp "test/regression/no-such-file.fasl"))
  (t nil nil nil))
