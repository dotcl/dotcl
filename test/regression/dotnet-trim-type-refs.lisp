;;; The project build collects the .NET type names a unit writes literally, so it
;;; can hand them to the trimmer (a fasl reaches .NET only through reflection and
;;; the trimmer would remove what it uses). compile-file-concatenated-collecting is
;;; the entry the C# build driver calls; it returns the names.
;;;
;;; Only type positions count: member names ("Hello", "GetYear") must not be
;;; collected, and a type name held in a variable cannot be.

(defvar *tt-tmp-dir* "test/regression/.tmp-trimrefs/")

(defun tt-collect (text)
  (ensure-directories-exist *tt-tmp-dir*)
  (let ((src  (namestring (merge-pathnames "tt-src.lisp" (truename *tt-tmp-dir*))))
        (fasl (namestring (merge-pathnames "tt.fasl"     (truename *tt-tmp-dir*)))))
    (with-open-file (s src :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
      (write-string text s))
    (sort (copy-list (dotcl.cil-compiler::compile-file-concatenated-collecting src fasl))
          #'string<)))

(deftest-compiled-only dotnet-trim-type-refs-positions
  (tt-collect
   "(in-package :cl-user)
    (defun tt-a (d n)
      (list (dotnet:new \"TT.Greeter\")
            (dotnet:static \"TT.Util\" \"Twice\" n)
            (dotnet:make-generic-type \"System.Collections.Generic.List\" (list \"TT.Item\"))
            (dotnet:static-generic \"TT.Gen\" \"Make\" '(\"TT.Arg\"))
            (dotnet:cast d \"TT.Cast\")
            (dotnet:invoke d \"Hello\" \"not-a-type\")
            (let ((name \"TT.Hidden\")) (dotnet:new name))))")
  ("System.Collections.Generic.List" "TT.Arg" "TT.Cast" "TT.Gen" "TT.Greeter"
   "TT.Item" "TT.Util"))

(deftest-compiled-only dotnet-trim-type-refs-setf-static
  ;; (setf (dotnet:static ...)) expands to another call; the type still counts.
  (tt-collect
   "(in-package :cl-user)
    (defun tt-b (v) (setf (dotnet:static \"TT.Settings\" \"Level\") v))")
  ("TT.Settings"))
