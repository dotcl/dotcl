;;;; hello-gui.lisp — a cross-platform GUI from plain Common Lisp.
;;;; Run with:  dotcl --load hello-gui.lisp

(require "dotnet-class")                         ; dotnet:define-class (ships with dotcl)
(require "nuget")                          ; NuGet resolver (ships with dotcl)
(nuget:require "Avalonia.Desktop" :version "12.0.4") ; pulls Avalonia + platform backends
(nuget:require "Avalonia.Themes.Fluent" :version "12.0.4")
(dotnet:load-assembly "Avalonia.Desktop")        ; UsePlatformDetect lives here
(dotnet:load-assembly "Avalonia.Themes.Fluent")

(dotnet:define-class "Hello.App" ("Avalonia.Application")
  (:ctor ()
    (dotnet:invoke (dotnet:invoke self "get_Styles") "Add"
                   (dotnet:new "Avalonia.Themes.Fluent.FluentTheme")))
  (:methods
    ("OnFrameworkInitializationCompleted" () :returns Void :override t
      (let ((win    (dotnet:new "Avalonia.Controls.Window"))
            (button (dotnet:new "Avalonia.Controls.Button"))
            (clicks 0))
        (dotnet:invoke win "set_Title" "Hello from Common Lisp")
        (dotnet:invoke win "set_Width" 420d0)
        (dotnet:invoke win "set_Height" 240d0)
        (dotnet:invoke button "set_Content" "Click me")
        (dotnet:invoke button "set_HorizontalAlignment"
                       (dotnet:static "Avalonia.Layout.HorizontalAlignment" "Center"))
        (dotnet:invoke button "set_VerticalAlignment"
                       (dotnet:static "Avalonia.Layout.VerticalAlignment" "Center"))
        (dotnet:add-event button "Click"
          (lambda (s e) (declare (ignore s e))
            (dotnet:invoke button "set_Content"
                           (format nil "~r click~:p from Lisp!" (incf clicks)))))
        (dotnet:invoke win "set_Content" button)
        (dotnet:invoke (dotnet:invoke self "get_ApplicationLifetime")
                       "set_MainWindow" win)))))

;;; Start the application on the process main thread. dotcl runs Lisp on a
;;; worker thread with a big stack, and macOS AppKit accepts UI work on the main
;;; thread only; CALL-ON-MAIN-THREAD puts the event loop back where the toolkit
;;; wants it and returns when the application exits. Windows and X11 do not care,
;;; so the wrapper costs nothing there.
(dotcl:call-on-main-thread
 (lambda ()
   (let* ((builder (dotnet:static-generic "Avalonia.AppBuilder" "Configure" (list "Hello.App")))
          (builder (dotnet:static "Avalonia.AppBuilderDesktopExtensions" "UsePlatformDetect" builder))
          (args    (dotnet:static-generic "System.Array" "Empty" (list "System.String"))))
     (dotnet:static "Avalonia.ClassicDesktopStyleApplicationLifetimeExtensions"
                    "StartWithClassicDesktopLifetime" builder args))))
