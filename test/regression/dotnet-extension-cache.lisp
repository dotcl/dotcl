;;; Extension methods from an assembly loaded later in the session.
;;;
;;; The index of extension methods is keyed by method name and filled the first
;;; time a name is asked for, by scanning the assemblies loaded at that moment.
;;; Nothing invalidated it, so a name probed before its assembly arrived stayed
;;; empty for the rest of the session -- and that is the normal order for a
;;; package pulled in with nuget:require, or for any assembly loaded on demand.
;;; The index is now dropped whenever an assembly loads.

(defun dec-ints ()
  (let ((list (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                    (list "System.Int32")))))
    (dotnet:invoke list "Add" 5)
    (dotnet:invoke list "Add" 6)
    list))

;;; Ask for the name while System.Linq.Queryable is not loaded. The call fails
;;; either way -- a StringBuilder is not IEnumerable -- but it is the lookup that
;;; matters: this is what used to cache an empty answer for good.
(deftest dec-probe-before-load-then-use
  (progn
    (ignore-errors (dotnet:invoke (dotnet:new "System.Text.StringBuilder") "AsQueryable"))
    (dotnet:load-assembly "System.Linq.Queryable")
    (dotnet:invoke (dotnet:invoke (dec-ints) "AsQueryable") "Count"))
  2)

;;; And once loaded it keeps working, i.e. the drop does not leave the index in a
;;; state where the next lookup misses.
(deftest dec-still-resolves-afterwards
  (dotnet:invoke (dotnet:invoke (dec-ints) "AsQueryable") "Count")
  2)
