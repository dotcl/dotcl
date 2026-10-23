# dotcl on Windows

Windows-specific behaviour worth knowing. General .NET interop examples live in
`examples/windows/`.

## Console I/O

- **UTF-8 always** for stdin/stdout -- independent of `chcp`, the parent code
  page, or `%LANG%`. Non-ASCII `format` / `read-line` works as-is. Garbled
  output is a terminal font issue, not dotcl (the bytes are valid UTF-8).
- **ANSI/VT escapes** (colour, cursor) are on at startup, even in legacy
  conhost / cmd.exe. Set `DOTCL_NO_VT=1` to disable.

## Pathnames

- Both `C:/foo` and `C:\foo` are accepted; `namestring` canonicalises to forward
  slashes (`(pathname "C:\\Users\\you")` -> `#P"C:/Users/you"`).
- `*default-pathname-defaults*` carries the drive letter. Wildcards,
  `probe-file`, `truename` (junction/symlink), and non-ASCII filenames all work.

## Calling Windows APIs

Use the `dotnet:` package. For example, reading the registry:

```lisp
(dotnet:static "Microsoft.Win32.Registry, Microsoft.Win32.Registry" "GetValue"
  "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"
  "ProductName" "unknown")
```

(the `, Microsoft.Win32.Registry` suffix is an assembly qualification.) APIs not
bundled with the runtime -- e.g. `System.Management` for WMI -- load with
`(dotnet:require "System.Management")`. More examples (Environment, WMI, SAPI,
StringBuilder) are in `examples/windows/dotnet-interop-examples.lisp`.

## UI frameworks (WinForms / WPF / MAUI / WinUI / MonoGame)

Frameworks that need subclassing (`Form`, `Application`, `ContentPage`, `Game`, ...)
use `dotnet:define-class` to emit a named .NET class from Lisp. UI must run on an
**STA thread** with a message pump (`Application.Run` for WinForms,
`Dispatcher.Run` for WPF).

- **WinForms** works from the plain `dotcl` runtime.
- **WPF** needs a **WindowsDesktop host** -- an embedder project targeting
  `net10.0-windows` with `<UseWPF>true</UseWPF>`, referencing `DotCL.Runtime`.
  The bare `net10.0` runtime loads the assemblies but trips on WPF's type
  forwarders.

Working samples: `samples/MauiLispDemo/` (MAUI), `samples/AspNetLispDemo/`
(ASP.NET Core), `samples/MonoGameLispDemo/` (MonoGame), and
`examples/windows/winforms-demo.lisp`.

## COM

COM objects are used late-bound, through IDispatch, with the ordinary `dotnet:`
calls. Everything below was run on a stock Windows 11 install (no Office, nothing
extra installed); the comments show what it returned there.

### Creating objects, calling methods, properties

`dotnet:new` takes a ProgID. `dotnet:invoke` calls a method or reads a property
by name, and `setf` of it writes a property:

```lisp
(dotnet:invoke (dotnet:new "WScript.Shell") "ExpandEnvironmentStrings" "%WINDIR%")
;; => "C:\\WINDOWS"

(defvar *fso* (dotnet:new "Scripting.FileSystemObject"))
(dotnet:invoke *fso* "FolderExists" "C:\\Windows")           ; => T

(let ((sh (dotnet:new "WScript.Shell")))
  (setf (dotnet:invoke sh "CurrentDirectory") "C:\\Windows")
  (dotnet:invoke sh "CurrentDirectory"))                     ; => "C:\\Windows"
```

Optional arguments can be left off the end:

```lisp
(let* ((tmp (dotnet:invoke (dotnet:invoke *fso* "GetSpecialFolder" 2) "Path"))
       (path (dotnet:invoke *fso* "BuildPath" tmp "dotcl-com.txt")))
  ;; OpenTextFile(path, iomode, create, format): 2 = ForWriting, format left off
  (let ((out (dotnet:invoke *fso* "OpenTextFile" path 2 t)))
    (dotnet:invoke out "WriteLine" "hello")
    (dotnet:invoke out "Close"))
  ;; iomode defaults to ForReading
  (let ((in (dotnet:invoke *fso* "OpenTextFile" path)))
    (prog1 (dotnet:invoke in "ReadLine")
      (dotnet:invoke in "Close")
      (dotnet:invoke *fso* "DeleteFile" path))))
;; => "hello"
```

Text to speech, into a WAV file:

```lisp
(let ((voice (dotnet:new "SAPI.SpVoice"))
      (wav (dotnet:new "SAPI.SpFileStream"))
      (path (concatenate 'string (dotnet:static "System.IO.Path" "GetTempPath") "hello.wav")))
  (dotnet:invoke wav "Open" path 3)                ; 3 = SSFMCreateForWrite
  (setf (dotnet:invoke voice "AudioOutputStream") wav)
  (dotnet:invoke voice "Speak" "Hello from Lisp")
  (dotnet:invoke wav "Close")
  path)
```

### Collections

A COM collection is enumerated with `GetEnumerator`, `MoveNext` and `Current`:

```lisp
(defun com-items (collection)
  (let ((e (dotnet:invoke collection "GetEnumerator")) (items '()))
    (loop while (dotnet:invoke e "MoveNext")
          do (push (dotnet:invoke e "Current") items))
    (nreverse items)))

(mapcar (lambda (d) (dotnet:invoke d "DriveLetter"))
        (com-items (dotnet:invoke *fso* "Drives")))         ; => ("C")
```

WMI is reachable the same way, without any package:

```lisp
(defvar *wmi* (dotnet:invoke (dotnet:new "WbemScripting.SWbemLocator")
                             "ConnectServer" "." "root\\cimv2"))

(mapcar (lambda (os) (dotnet:invoke os "Caption"))
        (com-items (dotnet:invoke *wmi* "ExecQuery"
                                  "SELECT Caption FROM Win32_OperatingSystem")))
;; => ("Microsoft Windows 11 Pro")
```

### Out parameters

IDispatch carries no signature, so `dotnet:call-out` passes every argument by
reference and returns the return value followed by each argument's value after
the call. WMI's registry provider writes its result into the fourth argument
(`-2147483646` is `HKEY_LOCAL_MACHINE`, `#x80000002`, as the signed 32-bit
value the method expects):

```lisp
(let ((reg (dotnet:invoke (dotnet:invoke (dotnet:new "WbemScripting.SWbemLocator")
                                         "ConnectServer" "." "root\\default")
                          "Get" "StdRegProv")))
  (multiple-value-bind (result hive key name value)
      (dotnet:call-out reg "GetStringValue" -2147483646
                       "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion" "CurrentBuild" nil)
    (declare (ignore hive key name))
    (list result value)))
;; => (0 "26300")
```

### Errors

A failed call signals an `error`. The .NET exception behind it, and through it the
HRESULT, are available from the condition:

```lisp
(handler-case (dotnet:invoke *fso* "GetFolder" "C:\\no\\such\\folder")
  (error (e)
    (list (dotnet:exception-type e)
          (format nil "~8,'0X"
                  (logand #xFFFFFFFF
                          (dotnet:invoke (dotnet:exception-object e) "HResult"))))))
;; => (#<DOTNET System.RuntimeType System.IO.DirectoryNotFoundException> "800A004C")
```

A member the object does not have is an `error` naming it
(`DOTNET:INVOKE __ComObject.NoSuchMethod: Unknown name. ...`).

### Releasing objects

A COM object is released when it is garbage collected, or at once with
`Marshal.FinalReleaseComObject` (or `ReleaseComObject`). An out-of-process server
exits once its last reference is released; using the object afterwards signals an
error ("COM object that has been separated from its underlying RCW cannot be
used"):

```lisp
(defun process-count (name)
  (dotnet:invoke (dotnet:static "System.Diagnostics.Process" "GetProcessesByName" name)
                 "Length"))

(let ((mmc (dotnet:new "MMC20.Application")))
  (list (process-count "mmc")
        (dotnet:static "System.Runtime.InteropServices.Marshal" "FinalReleaseComObject" mmc)
        (progn (sleep 3) (process-count "mmc"))))
;; => (1 0 0)
```

### Events

A COM event source calls back through a connection point. The sink is a
`dotnet:define-class` object that answers `QueryInterface` for the event
interface with its own IDispatch; its methods carry the events' DISPIDs:

```lisp
(require "dotnet-class")

(defmacro define-event-sink (class-name event-iid &body handlers)
  `(dotnet:define-class ,class-name ()
     (:implements "System.Runtime.InteropServices.ICustomQueryInterface")
     (:methods
       ("GetInterface" ((iid "System.Guid&") (ppv "System.IntPtr&"))
        :returns "System.Runtime.InteropServices.CustomQueryInterfaceResult"
        (cond ((string-equal (dotnet:invoke (dotnet:deref iid) "ToString") ,event-iid)
               (setf (dotnet:deref ppv)
                     (dotnet:static "System.Runtime.InteropServices.Marshal"
                                    "GetIDispatchForObject" self))
               (dotnet:static "System.Runtime.InteropServices.CustomQueryInterfaceResult" "Handled"))
              (t (dotnet:static "System.Runtime.InteropServices.CustomQueryInterfaceResult"
                                "NotHandled"))))
       ,@(loop for (name dispid params . body) in handlers
               collect `(,name ,params :returns "System.Void"
                         :attributes (("System.Runtime.InteropServices.DispIdAttribute" ,dispid))
                         ,@body)))))

(defun advise (source event-iid sink)
  "Connect SINK to SOURCE's events. Returns the connection point and a cookie."
  (let* ((container (dotnet:cast source "System.Runtime.InteropServices.ComTypes.IConnectionPointContainer"))
         (point (dotnet:cast (nth-value 2 (dotnet:call-out container "FindConnectionPoint"
                                                           (dotnet:new "System.Guid" event-iid)))
                             "System.Runtime.InteropServices.ComTypes.IConnectionPoint")))
    (values point (nth-value 1 (dotnet:call-out point "Advise" sink)))))

(defun unadvise (point cookie)
  (dotnet:invoke point "Unadvise" cookie))
```

An asynchronous WMI query delivers each result as an event (the IID is
`ISWbemSinkEvents`):

```lisp
(defvar *found* '())
(defvar *done* nil)

(define-event-sink "MyApp.WmiSink" "75718ca0-f029-11d1-a1ac-00c04fb6c223"
  ("OnObjectReady" 1 ((obj "System.Object") (context "System.Object"))
    (push (dotnet:invoke obj "Name") *found*))
  ("OnCompleted" 2 ((hresult "System.Int32") (err "System.Object") (context "System.Object"))
    (setf *done* t)))

(let ((source (dotnet:new "WbemScripting.SWbemSink")))
  (multiple-value-bind (point cookie)
      (advise source "75718ca0-f029-11d1-a1ac-00c04fb6c223" (dotnet:new "MyApp.WmiSink"))
    (dotnet:invoke *wmi* "ExecQueryAsync" source
                   "SELECT Name FROM Win32_Process WHERE Name = 'winlogon.exe'")
    (loop repeat 100 until *done* do (sleep 0.05))
    (unadvise point cookie)
    *found*))
;; => ("winlogon.exe")
```

WMI calls the sink on a COM thread of its own. A Lisp error inside a sink method
is reported on `*error-output*` and not passed back to the event source.

### Apartments and threads

Lisp threads, the main one included, are in the multithreaded apartment (MTA).
Objects that live in a single-threaded apartment (STA) deliver their calls and
events through the message queue of the thread that created them, so that thread
has to pump messages. `dotnet:ui-invoke` runs a function on an STA thread that
does (it needs `System.Windows.Forms` loaded first).

SAPI's voice events are one such case. Created on the main Lisp thread, a voice
speaks but its events never arrive; created on the pumping thread, they do:

```lisp
(defvar *words* '())
(defvar *ended* nil)

;; _ISpeechVoiceEvents
(define-event-sink "MyApp.VoiceSink" "a372acd1-3bef-4bbd-8ffb-cb3e2b416af8"
  ("Word" 7 ((stream "System.Object") (position "System.Object")
             (start "System.Object") (length "System.Object"))
    (push (list start length) *words*))
  ("EndStream" 2 ((stream "System.Object") (position "System.Object"))
    (setf *ended* t)))

(dotnet:load-assembly "System.Windows.Forms")

(dotnet:ui-invoke
 (lambda ()
   (let ((voice (dotnet:new "SAPI.SpVoice")))
     (setf (dotnet:invoke voice "AudioOutputStream") (dotnet:new "SAPI.SpMemoryStream"))
     (advise voice "a372acd1-3bef-4bbd-8ffb-cb3e2b416af8" (dotnet:new "MyApp.VoiceSink"))
     (dotnet:invoke voice "Speak" "events from the voice" 1))))   ; 1 = SVSFlagsAsync

(loop repeat 200 until *ended* do (sleep 0.05))
(reverse *words*)                                                 ; => ((0 21))
```

An object created on an STA thread can be called from other threads while that
thread pumps (the call is marshalled to it). If the thread is busy and does not
pump, such a call blocks; if the thread has exited, the object can no longer be
used.
