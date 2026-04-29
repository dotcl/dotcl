;;;; Windows .NET Interop Examples for dotcl
;;;;
;;;; Load via ASDF:
;;;;   (require "asdf")
;;;;   (push #p"/path/to/examples/windows/" asdf:*central-registry*)
;;;;   (asdf:load-system "windows-examples")
;;;;   (windows-examples:registry-demo)
;;;;   (windows-examples:wmi-demo)

(in-package :windows-examples)

(defun registry-demo ()
  "Read a value from the Windows Registry."
  (let ((name (dotnet:static "Microsoft.Win32.Registry, Microsoft.Win32.Registry"
                             "GetValue"
                             "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"
                             "ProductName"
                             "unknown")))
    (format t "Windows ProductName: ~a~%" name)
    name))

(defun environment-demo ()
  "Show system environment info via dotnet:static."
  (format t "Temp directory:  ~a~%" (dotnet:static "System.IO.Path" "GetTempPath"))
  (format t "MachineName:     ~a~%" (dotnet:static "System.Environment" "MachineName"))
  (format t "Path separator:  ~a~%" (dotnet:static "System.IO.Path" "DirectorySeparatorChar")))

(defun string-builder-demo ()
  "Demonstrate StringBuilder instance access via dotnet:invoke and setf."
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" "hello")
    (dotnet:invoke sb "Append" " world")
    (format t "ToString: ~a~%" (dotnet:invoke sb "ToString"))
    (format t "Length:   ~a~%" (dotnet:invoke sb "Length"))
    (setf (dotnet:invoke sb "Length") 5)
    (format t "Truncated: ~a~%" (dotnet:invoke sb "ToString"))))

(defun wmi-demo ()
  "Query OS info via WMI (downloads System.Management from NuGet on first run)."
  (dotnet:require "System.Management")
  (let* ((searcher (dotnet:new "System.Management.ManagementObjectSearcher, System.Management"
                               "SELECT Caption,Version FROM Win32_OperatingSystem"))
         (results  (dotnet:invoke searcher "Get"))
         (enum     (dotnet:invoke results "GetEnumerator")))
    (dotnet:invoke enum "MoveNext")
    (let ((obj (dotnet:invoke enum "get_Current")))
      (format t "OS Caption: ~a~%" (dotnet:invoke obj "get_Item" "Caption"))
      (format t "OS Version: ~a~%" (dotnet:invoke obj "get_Item" "Version")))))

(defun com-sapi-demo ()
  "Speak a phrase via SAPI.SpVoice COM object (requires Windows Speech Platform)."
  (let ((voice (dotnet:new "SAPI.SpVoice")))
    (dotnet:invoke voice "Speak" "Hello from dotcl" 1)
    (format t "SAPI.SpVoice: speaking asynchronously~%")))
