;;; Where the multi-core JIT profile is written.
;;;
;;; The runtime writes a profile of the methods a run compiles, and reads it
;;; back on the next run to compile them ahead on a background thread. It used
;;; to land beside the executing assembly. That is the one place it cannot go:
;;; an executable built by SAVE-APPLICATION keeps dotcl's entry point, so a
;;; shipped application wrote a dotcl-named file into its own install
;;; directory, silently failed to under a read-only install, and left copies in
;;; publish directories that `dotnet pack` then shipped inside the package.
;;;
;;; Writing the profile cannot be observed without a real start, so what is
;;; pinned here is the rule that decides the path. Two things have to hold.
;;; The location is the user's cache home, the same one the fasl cache uses,
;;; and never the install directory. And the name is per executable: the
;;; runtime overwrites the profile with the current run's trace, so if two
;;; programs shared a file each would leave the other a list of methods it
;;; never calls, and the background thread would spend the startup window
;;; compiling them.

(require "asdf")

(defvar *jpp-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defun %jpp-dir (path)
  "The directory part of PATH, with the trailing slash kept."
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path 0 (1+ slash)) "")))

(defun %jpp-slashes (path)
  (substitute #\/ #\\ path))

;;; The profile belongs in the user's cache home, in dotcl's own corner of it.
;;; Not under common-lisp/, which is uiop's shared namespace and which
;;; `dotcl clean` empties by deleting every dotcl-* directory it finds there.
(deftest jit-profile-path.under-cache-home
  (let ((dir (%jpp-dir (dotcl::%jit-profile-path)))
        (want (%jpp-slashes (namestring (uiop:xdg-cache-home "dotcl/jit/")))))
    (string= dir want))
  t)

;;; The whole point: not beside the executable.
(deftest jit-profile-path.not-beside-the-executable
  (string= (%jpp-dir (dotcl::%jit-profile-path))
           (%jpp-dir (%jpp-slashes *jpp-exe*)))
  nil)

;;; Named like a profile, and the same name on every run: the hash in it must
;;; not be a per-process seed, or each start would write a fresh file and read
;;; none of them back.
(deftest jit-profile-path.stable-and-suffixed
  (let ((a (dotcl::%jit-profile-path))
        (b (dotcl::%jit-profile-path)))
    (list (string= a b)
          (let ((n (length a)))
            (and (> n 8) (string= (subseq a (- n 8)) ".profile")))))
  (t t))

;;; Two programs never share a profile, however alike they look. Both cases
;;; arise. SAVE-APPLICATION copies one and the same runtime.exe to whatever the
;;; user called it and never renames the assembly, so two installations of one
;;; application differ only in their directory, and a pair of tools shipped
;;; into a single directory differ only in their file name.
(deftest jit-profile-name.distinguishes-programs
  (flet ((n (exe dir) (dotcl::%jit-profile-name-for exe dir)))
    (list (string= (n "/opt/one/app.exe" "/opt/one/") (n "/opt/two/app.exe" "/opt/two/"))
          (string= (n "/opt/one/app.exe" "/opt/one/") (n "/opt/one/other.exe" "/opt/one/"))
          (string= (n "/opt/one/app.exe" "/opt/one/") (n "/opt/one/app.exe" "/opt/one/"))))
  (nil nil t))

;;; Run as `dotnet whatever.dll` the executable is the shared dotnet host, one
;;; path for every .NET program on the machine. Keying on it would give every
;;; dotcl build ever launched that way a single shared profile, each run
;;; leaving the next a trace of the wrong program. The assembly directory is
;;; what tells them apart, so it is what the name is built from.
(deftest jit-profile-name.shared-host-does-not-collapse
  (let ((host "/usr/share/dotnet/dotnet"))
    (string= (dotcl::%jit-profile-name-for host "/build/a/")
             (dotcl::%jit-profile-name-for host "/build/b/")))
  nil)

;;; A trailing separator is a spelling of the same directory, not another one.
(deftest jit-profile-name.directory-spelling-is-not-identity
  (string= (dotcl::%jit-profile-name-for "/opt/one/app.exe" "/opt/one")
           (dotcl::%jit-profile-name-for "/opt/one/app.exe" "/opt/one/"))
  t)

;;; The readable half of the name is the program's, so the directory can be
;;; made sense of by eye. That is what the old file was missing: a user found a
;;; dotcl.profile next to their own program and had to ask what wrote it.
(deftest jit-profile-name.carries-the-program-name
  (let ((name (dotcl::%jit-profile-name-for "/opt/one/myapp.exe" "/opt/one/")))
    (and (eql 0 (search "myapp-" name)) t))
  t)

;;; Nothing to go on still yields a usable name rather than an error: this runs
;;; before there is any way to report one.
(deftest jit-profile-name.tolerates-nothing
  (list (dotcl::%jit-profile-name-for "" "")
        (dotcl::%jit-profile-name-for "   " "  "))
  ("dotcl-unknown.profile" "dotcl-unknown.profile"))
