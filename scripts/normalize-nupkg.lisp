;;; Rewrite .nupkg files so the zip itself is a function of its contents.
;;;
;;; `dotnet pack` stamps every entry with the time of the pack, so two packs of the same
;;; inputs differ in the zip headers (the files inside are the same). This rewrites each
;;; package in place with a fixed time on every entry and the entries in their original
;;; order. Nothing inside an entry changes.
;;;
;;; Usage: dotcl scripts/normalize-nupkg.lisp FILE.nupkg [FILE.nupkg ...]

(defun entry-bytes (entry)
  (let ((in (dotnet:invoke entry "Open"))
        (ms (dotnet:new "System.IO.MemoryStream")))
    (dotnet:invoke in "CopyTo" ms)
    (dotnet:invoke in "Dispose")
    (dotnet:invoke ms "ToArray")))

(defun normalize (path)
  (let* ((tmp (concatenate 'string path ".normalizing"))
         (src (dotnet:static "System.IO.Compression.ZipFile" "OpenRead" path))
         (entries (let ((es (dotnet:invoke src "Entries")))
                    (loop for i below (dotnet:invoke es "Count")
                          for e = (dotnet:invoke es "get_Item" i)
                          collect (cons (dotnet:invoke e "FullName") (entry-bytes e)))))
         ;; the earliest time a zip can store
         (fixed (dotnet:new "System.DateTimeOffset" 1980 1 1 0 0 0 (dotnet:static "System.TimeSpan" "Zero")))
         (file (dotnet:static "System.IO.File" "Create" tmp))
         (dst (dotnet:new "System.IO.Compression.ZipArchive" file
                          (dotnet:static "System.IO.Compression.ZipArchiveMode" "Create")
                          nil)))
    (dotnet:invoke src "Dispose")
    (dolist (e entries)
      (let* ((entry (dotnet:invoke dst "CreateEntry" (car e)
                                   (dotnet:static "System.IO.Compression.CompressionLevel" "Optimal")))
             (bytes (cdr e)))
        ;; the time and attributes have to be set before the entry is opened for writing
        (setf (dotnet:invoke entry "LastWriteTime") fixed)
        (setf (dotnet:invoke entry "ExternalAttributes") (ash #o644 16))
        (let ((out (dotnet:invoke entry "Open")))
        (dotnet:invoke out "Write" bytes 0 (dotnet:invoke bytes "Length"))
        (dotnet:invoke out "Dispose"))))
    (dotnet:invoke dst "Dispose")
    (dotnet:invoke file "Dispose")
    (dotnet:static "System.IO.File" "Move" tmp path t)))

(dolist (path (dotcl:script-arguments))
  (normalize path))
