;;; Where dotcl looks for the user init file on Unix and macOS.
;;;
;;; $XDG_CONFIG_HOME/dotcl/init.lisp, defaulting to ~/.config/dotcl/init.lisp,
;;; on Linux and macOS alike. The path used to come from .NET's
;;; SpecialFolder.ApplicationData, which is ~/.config on Linux but
;;; ~/Library/Application Support on macOS, so the documented path was not the
;;; one read there. An init file left at that old macOS location is still
;;; loaded until one exists at the XDG path.
;;;
;;; Each case runs this executable again with HOME and XDG_CONFIG_HOME pointed
;;; into a scratch directory, and checks both what DOTCL:USER-INIT-FILE answers
;;; and that the file there is the one actually loaded. Windows keeps
;;; %APPDATA% and ignores both variables, so the cases only assert there that
;;; the child runs.

(defvar *uif-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *uif-core*
  (regression-child-core))

(defvar *uif-dir*
  (concatenate 'string (regression-temp-dir) "/user-init-file/"))

(defun %uif-windows-p ()
  (dotnet:static "System.OperatingSystem" "IsWindows"))

(defun %uif-macos-p ()
  (dotnet:static "System.OperatingSystem" "IsMacOS"))

(defun %uif-write-init (path marker)
  "Write an init file at PATH that prints MARKER when loaded."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "(format t \"~~&LOADED=~a~~%\")~%" marker))
  path)

(defun %uif-run (home xdg)
  "Run the CLI with HOME and XDG_CONFIG_HOME set (\"\" for unset). Returns
   (exit stdout stderr)."
  (let ((saved-home (dotcl:getenv "HOME"))
        (saved-xdg (dotcl:getenv "XDG_CONFIG_HOME")))
    (unwind-protect
         (progn (dotcl:setenv "HOME" home)
                (dotcl:setenv "XDG_CONFIG_HOME" xdg)
                (dotcl:run-process
                 *uif-exe*
                 (list "--core" *uif-core* "--eval"
                       "(format t \"~&INIT=~a~%\" (namestring (dotcl:user-init-file)))")))
      (dotcl:setenv "HOME" (or saved-home ""))
      (dotcl:setenv "XDG_CONFIG_HOME" (or saved-xdg "")))))

(defun %uif-case (name)
  "A fresh scratch directory for one case: the parent is this process's own."
  (let ((dir (concatenate 'string *uif-dir* name "/")))
    (ensure-directories-exist dir)
    dir))

(defun %uif-check (result expected-path marker)
  "On Windows only that the child ran; elsewhere that it reported EXPECTED-PATH
   and loaded the init file carrying MARKER."
  (let ((out (second result)))
    (if (%uif-windows-p)
        (first result)
        (list (first result)
              (and (search (concatenate 'string "INIT=" expected-path) out) t)
              (and marker
                   (search (concatenate 'string "LOADED=" marker) out)
                   t)))))

(defun %uif-expected (&optional (marker-p t))
  (if (%uif-windows-p) 0 (list 0 t marker-p)))

;;; XDG_CONFIG_HOME, when absolute, is the config root.
(deftest user-init-file.xdg-config-home
  (let* ((dir (%uif-case "xdg"))
         (path (concatenate 'string dir "xdg/dotcl/init.lisp")))
    (%uif-write-init path "xdg")
    (equal (%uif-check (%uif-run (concatenate 'string dir "home")
                                 (concatenate 'string dir "xdg"))
                       path "xdg")
           (%uif-expected)))
  t)

;;; Unset: ~/.config, on macOS as on Linux.
(deftest user-init-file.default-dot-config
  (let* ((dir (%uif-case "default"))
         (path (concatenate 'string dir "home/.config/dotcl/init.lisp")))
    (%uif-write-init path "dotconfig")
    (equal (%uif-check (%uif-run (concatenate 'string dir "home") "")
                       path "dotconfig")
           (%uif-expected)))
  t)

;;; A relative XDG_CONFIG_HOME is invalid and ignored.
(deftest user-init-file.relative-xdg-ignored
  (let* ((dir (%uif-case "relative"))
         (path (concatenate 'string dir "home/.config/dotcl/init.lisp")))
    (%uif-write-init path "relative")
    (equal (%uif-check (%uif-run (concatenate 'string dir "home") "rel/xdg")
                       path "relative")
           (%uif-expected)))
  t)

;;; No init file anywhere: the XDG path is still the answer, so that
;;; quicklisp's add-to-init-file writes to the new location.
(deftest user-init-file.none-answers-xdg
  (let* ((dir (%uif-case "none"))
         (path (concatenate 'string dir "home/.config/dotcl/init.lisp")))
    (equal (%uif-check (%uif-run (concatenate 'string dir "home") "")
                       path nil)
           (%uif-expected nil)))
  t)

;;; macOS: an init file at the old location is still read while there is none
;;; at the XDG path, and loses to one that is.
(deftest user-init-file.macos-legacy-fallback
  (if (%uif-macos-p)
      (let* ((dir (%uif-case "legacy"))
             (home (concatenate 'string dir "home"))
             (legacy (concatenate 'string home
                                  "/Library/Application Support/dotcl/init.lisp"))
             (xdg (concatenate 'string home "/.config/dotcl/init.lisp")))
        (%uif-write-init legacy "legacy")
        (let ((before (%uif-check (%uif-run home "") legacy "legacy")))
          (%uif-write-init xdg "xdgwins")
          (let ((after (%uif-check (%uif-run home "") xdg "xdgwins")))
            (and (equal before '(0 t t))
                 (equal after '(0 t t))
                 (not (search "LOADED=legacy"
                              (second (%uif-run home ""))))))))
      t)
  t)
