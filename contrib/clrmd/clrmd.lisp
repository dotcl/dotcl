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
           #:heap-report #:heap-histogram #:symbols-by-package
           #:functions-by-name #:list-stats
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

;;; ------------------------------------------------------------------
;;; Heap analysis in Lisp terms — the serious version of ROOM.
;;;
;;; A .NET heap profiler answers "how many Cons / Symbol / LispVector", which is
;;; one level below the question you actually have. These aggregate the same walk
;;; into Lisp terms: which package the symbols belong to (an intern leak shows up
;;; as one package dwarfing the rest), how the conses are arranged into lists,
;;; which functions are on the heap.
;;;
;;;   (clrmd:heap-report)              ; histogram + symbols by package + functions
;;;   (clrmd:heap-report :lists t)     ; also walk cdr chains (slower, see below)
;;;
;;; Cost on a ~1.8M object process: ~6s for the type pass, ~+15s for the cdr
;;; pass. Every field read crosses the interop boundary, so it is linear in
;;; objects with a large constant — fine for a diagnostic you run when a number
;;; surprises you, not for anything in a loop.
;;;
;;; Auto-properties are read through their compiler-generated backing field
;;; (`<Cdr>k__BackingField`), not the property name. Reading the property name
;;; does not error, it just returns nothing — which silently looks like a fast
;;; pass over an empty heap.

(defun %type-name (obj)
  (let ((ty (dotnet:invoke obj "get_Type")))
    (and ty (dotnet:invoke ty "get_Name"))))

(defun %heap-objects (rt)
  "Materialize the heap as an indexable list. EnumerateObjects returns a
compiler-generated iterator whose GetEnumerator is an explicit interface
implementation (invisible to name-based interop), hence the ToList."
  (let* ((heap (dotnet:invoke rt "get_Heap"))
         (objs (dotnet:invoke heap "EnumerateObjects")))
    (dotnet:static-generic "System.Linq.Enumerable" "ToList"
                           '("Microsoft.Diagnostics.Runtime.ClrObject") objs)))

(defun %bump (ht key count bytes)
  (let ((cur (gethash key ht)))
    (if cur
        (setf (car cur) (+ (car cur) count)
              (cdr cur) (+ (cdr cur) bytes))
        (setf (gethash key ht) (cons count bytes)))))

(defun %rank (ht &optional limit)
  "HT (key -> (count . bytes)) as a list of (key count bytes), biggest first."
  (let ((rows '()))
    (maphash (lambda (k v) (push (list k (car v) (cdr v)) rows)) ht)
    (setf rows (sort rows #'> :key #'second))
    (if limit (subseq rows 0 (min limit (length rows))) rows)))

(defun %print-rank (title rows)
  (format t "~&~%--- ~a ---~%" title)
  (dolist (r rows)
    (format t "~&~10:d  ~8,1F MB  ~a~%" (second r) (/ (third r) 1048576.0) (first r))))

(defun %symbol-package-name (obj)
  (let ((pkg (ignore-errors
              (dotnet:invoke obj "ReadObjectField" "<HomePackage>k__BackingField"))))
    (or (and pkg (ignore-errors
                  (dotnet:invoke pkg "ReadStringField" "<Name>k__BackingField")))
        "#<uninterned>")))

(defun %function-name (obj)
  (or (ignore-errors (dotnet:invoke obj "ReadStringField" "<Name>k__BackingField"))
      ;; Closures keep Name nil on purpose (a non-nil Name makes the slow-invoke
      ;; path push a call frame), so the defun they came from is not stored on
      ;; the LispFunction at all — it lives in the wrapper lambda's captured
      ;; display class. Whether that is reachable from here is untested; until
      ;; it is, they are counted together rather than guessed at.
      "#<closure (name not stored)>"))

(defun %list-stats (lst count)
  "Follow cdr chains. Returns (values list-count total-cells longest).
A list head is a cons no other cons points to with its cdr, so this needs the
whole cdr map before it can answer anything — hence two passes over the conses
rather than one."
  (let ((cdr-of (make-hash-table :test 'eql))
        (interior (make-hash-table :test 'eql)))
    (dotimes (i count)
      (let ((o (dotnet:invoke lst "get_Item" i)))
        (when (equal (%type-name o) "DotCL.Cons")
          (let* ((addr (dotnet:invoke o "get_Address"))
                 (cdr-obj (ignore-errors
                           (dotnet:invoke o "ReadObjectField" "<Cdr>k__BackingField")))
                 (cdr-addr (and cdr-obj
                                (ignore-errors (dotnet:invoke cdr-obj "get_Address")))))
            (setf (gethash addr cdr-of) (or cdr-addr 0))))))
    ;; A cdr that is itself a cons makes its target an interior cell.
    (maphash (lambda (addr cdr-addr)
               (declare (ignore addr))
               (when (and cdr-addr (/= cdr-addr 0) (nth-value 1 (gethash cdr-addr cdr-of)))
                 (setf (gethash cdr-addr interior) t)))
             cdr-of)
    (let ((heads 0) (longest 0) (cells 0))
      (maphash (lambda (addr cdr-addr)
                 (declare (ignore cdr-addr))
                 (incf cells)
                 (unless (gethash addr interior)
                   (incf heads)
                   ;; Walk to the end. The step cap keeps a circular structure
                   ;; (which a heap walk cannot rule out) from hanging the report.
                   (let ((n 0) (cur addr))
                     (loop
                       (incf n)
                       (let ((next (gethash cur cdr-of)))
                         (when (or (null next) (= next 0) (> n 10000000)) (return))
                         (unless (nth-value 1 (gethash next cdr-of)) (return))
                         (setf cur next)))
                     (when (> n longest) (setf longest n)))))
               cdr-of)
      (values heads cells longest))))

(defun heap-report (&key (top 20) (lists nil))
  "Print a heap summary in Lisp terms. One snapshot, one walk (plus a second
cons pass when LISTS is true)."
  (with-runtime (rt)
    (let ((types (make-hash-table :test 'equal))
          (pkgs  (make-hash-table :test 'equal))
          (fns   (make-hash-table :test 'equal))
          (start (get-internal-real-time)))
      (let* ((lst (%heap-objects rt))
             (count (dotnet:invoke lst "get_Count")))
        (dotimes (i count)
          (let* ((o (dotnet:invoke lst "get_Item" i))
                 (name (%type-name o)))
            (when name
              (let ((size (dotnet:invoke o "get_Size")))
                (%bump types name 1 size)
                (cond ((string= name "DotCL.Symbol")
                       (%bump pkgs (%symbol-package-name o) 1 size))
                      ((string= name "DotCL.LispFunction")
                       (%bump fns (%function-name o) 1 size)))))))
        (format t "~&~:d objects in ~d ms~%" count
                (round (* 1000 (- (get-internal-real-time) start))
                       internal-time-units-per-second))
        (%print-rank "type histogram" (%rank types top))
        (%print-rank "symbols by home package" (%rank pkgs top))
        (%print-rank "functions by name" (%rank fns top))
        (when lists
          (let ((t0 (get-internal-real-time)))
            (multiple-value-bind (heads cells longest) (%list-stats lst count)
              (format t "~&~%--- list structure ---~%")
              (format t "~&~10:d  lists (conses nothing else cdrs into)~%" heads)
              (format t "~&~10:d  cons cells total~%" cells)
              (format t "~&~10:d  longest chain~%" longest)
              (format t "~&           ~d ms~%"
                      (round (* 1000 (- (get-internal-real-time) t0))
                             internal-time-units-per-second)))))
        (values)))))

(defun heap-histogram (&key (top 20))
  "Live objects by .NET type, biggest first: a list of (type-name count bytes)."
  (with-runtime (rt)
    (let ((types (make-hash-table :test 'equal)))
      (let* ((lst (%heap-objects rt))
             (count (dotnet:invoke lst "get_Count")))
        (dotimes (i count)
          (let* ((o (dotnet:invoke lst "get_Item" i))
                 (name (%type-name o)))
            (when name (%bump types name 1 (dotnet:invoke o "get_Size"))))))
      (%rank types top))))

(defun symbols-by-package ()
  "Live symbols grouped by home package: (package-name count bytes), biggest
first. One package dwarfing the others is the usual shape of an intern leak."
  (with-runtime (rt)
    (let ((pkgs (make-hash-table :test 'equal)))
      (let* ((lst (%heap-objects rt))
             (count (dotnet:invoke lst "get_Count")))
        (dotimes (i count)
          (let ((o (dotnet:invoke lst "get_Item" i)))
            (when (equal (%type-name o) "DotCL.Symbol")
              (%bump pkgs (%symbol-package-name o) 1 (dotnet:invoke o "get_Size"))))))
      (%rank pkgs nil))))

(defun functions-by-name (&key (top 20))
  "Live LispFunctions by name: (name count bytes), biggest first. Closures are
pooled under one entry — see %function-name."
  (with-runtime (rt)
    (let ((fns (make-hash-table :test 'equal)))
      (let* ((lst (%heap-objects rt))
             (count (dotnet:invoke lst "get_Count")))
        (dotimes (i count)
          (let ((o (dotnet:invoke lst "get_Item" i)))
            (when (equal (%type-name o) "DotCL.LispFunction")
              (%bump fns (%function-name o) 1 (dotnet:invoke o "get_Size"))))))
      (%rank fns top))))

(defun list-stats ()
  "Returns (values list-count total-cells longest-chain) over the live heap."
  (with-runtime (rt)
    (let* ((lst (%heap-objects rt))
           (count (dotnet:invoke lst "get_Count")))
      (%list-stats lst count))))

(provide "clrmd")
