;;; clrmd.lisp — walk the managed heap of the running process, from Lisp.
;;;
;;; Wraps Microsoft.Diagnostics.Runtime (ClrMD — the engine inside WinDbg SOS /
;;; dotnet-dump) so you can ask questions dotcl can't answer by evaluating forms
;;; in-process: "which live objects of type X exist right now", and (later) what
;;; holds them and what the threads are doing. The "observe" layer of the probe
;;; loop (its read/patch companions are `decompiler` and `advice`); Arthas's
;;; vmtool getInstances / heapdump equivalent.
;;;
;;;   (require "clrmd")
;;;   (clrmd:instances-of "MyApp.OrderService")  ; addresses of live instances
;;;   (clrmd:count-of     "MyApp.OrderService")  ; just how many
;;;
;;; It snapshots the *current* process (ClrMD's most reliable mode — no dac/SOS
;;; version matching, no second process) and walks that frozen copy, so the walk
;;; never sees a half-mutated heap. A dump-file mode is a later addition.

(require "nuget")

(defpackage :clrmd
  (:use :cl)
  (:export #:instances-of #:count-of #:with-runtime #:ensure
           #:*package-name* #:*package-version*))

(in-package :clrmd)

(defvar *package-name* "Microsoft.Diagnostics.Runtime"
  "NuGet package providing ClrMD.")

(defvar *package-version* "*"
  "Version of *package-name* to resolve. \"*\" = latest stable.")

(defvar *ready* nil "T once ClrMD is resolvable.")

(defun ensure ()
  "Idempotently make Microsoft.Diagnostics.Runtime resolvable (load if already
present, else resolve from NuGet — the latter shells out to `dotnet build`)."
  (unless *ready*
    (or (ignore-errors (dotnet:load-assembly "Microsoft.Diagnostics.Runtime"))
        (progn
          (nuget:require *package-name* :version *package-version*)
          (dotnet:load-assembly "Microsoft.Diagnostics.Runtime")))
    (setf *ready* t)))

(defmacro with-runtime ((rt) &body body)
  "Snapshot-and-attach the current process, bind RT to a fresh ClrRuntime for
BODY, and dispose the snapshot afterwards. The heavy part (snapshot + attach) is
done once per call; keep BODY's heap walk inside the dynamic extent."
  (let ((dt (gensym "DT")) (pid (gensym "PID")))
    `(progn
       (ensure)
       (let* ((,pid (dotnet:invoke (dotnet:static "System.Diagnostics.Process"
                                                   "GetCurrentProcess") "get_Id"))
              (,dt  (dotnet:static "Microsoft.Diagnostics.Runtime.DataTarget"
                                   "CreateSnapshotAndAttach" ,pid)))
         (unwind-protect
              (let ((,rt (dotnet:invoke
                          (dotnet:invoke (dotnet:invoke ,dt "get_ClrVersions") "get_Item" 0)
                          "CreateRuntime")))
                ,@body)
           (dotnet:invoke ,dt "Dispose"))))))

(defun %map-instances (rt type-name fn)
  "Walk RT's heap, calling FN on the address (a Lisp integer) of every live
object whose exact type name is TYPE-NAME. Returns the number matched.

ClrHeap.EnumerateObjects returns a compiler-generated iterator whose
GetEnumerator is an explicit interface implementation (invisible to name-based
interop), so materialize it once with Enumerable.ToList<ClrObject> and index the
list instead."
  (let* ((heap  (dotnet:invoke rt "get_Heap"))
         (objs  (dotnet:invoke heap "EnumerateObjects"))
         (lst   (dotnet:static-generic "System.Linq.Enumerable" "ToList"
                                       '("Microsoft.Diagnostics.Runtime.ClrObject") objs))
         (count (dotnet:invoke lst "get_Count"))
         (n 0))
    (dotimes (i count)
      (let* ((obj (dotnet:invoke lst "get_Item" i))
             (ty  (dotnet:invoke obj "get_Type")))
        (when (and ty (string= (dotnet:invoke ty "get_Name") type-name))
          (incf n)
          (when fn (funcall fn (dotnet:invoke obj "get_Address"))))))
    n))

(defun instances-of (type-name)
  "Return a list of the heap addresses (Lisp integers) of every live object whose
exact type name is TYPE-NAME, in the running process."
  (with-runtime (rt)
    (let ((acc '()))
      (%map-instances rt type-name (lambda (addr) (push addr acc)))
      (nreverse acc))))

(defun count-of (type-name)
  "Return how many live objects of exact type TYPE-NAME exist — cheaper than
INSTANCES-OF when only the count matters."
  (with-runtime (rt)
    (%map-instances rt type-name nil)))

(provide "clrmd")
