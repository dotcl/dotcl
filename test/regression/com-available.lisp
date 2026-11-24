;;; Helpers for the tests that use COM objects (com-interop.lisp, call-out.lisp). A test asks
;;; whether the COM it needs is there by trying it, not by looking at the operating system or an
;;; environment variable: some Windows installations (a validation image, a server core) lack a
;;; class or the WMI service. When it is not there the test answers its expected value without
;;; running and one line says why.

(defun %com-windows-p () (dotnet:static "System.OperatingSystem" "IsWindows"))

(defun %com-skip (test reason)
  (format t "SKIP ~A: ~A~%" test reason)
  nil)

(defun %com-error-line (e)
  (let ((s (princ-to-string e)))
    (subseq s 0 (or (position #\Newline s) (length s)))))

(defun %com-ready-p (test &rest progids)
  "True when this is Windows and every ProgID in PROGIDS can be created (CoCreateInstance through
dotnet:new). Otherwise NIL after printing why."
  (cond ((not (%com-windows-p)) (%com-skip test "not Windows"))
        (t (dolist (id progids t)
             (handler-case (dotnet:new id)
               (error (e)
                 (return (%com-skip test (format nil "COM class ~A cannot be created (~A)" id (%com-error-line e))))))))))

(defun %com-ready-list (test &rest progids)
  "The ProgIDs among PROGIDS that can be created, on Windows. When there is none, NIL after printing
why each one cannot (one SKIP line per class), so a test that only needs one late-bound COM class
runs on every Windows image that has any of them."
  (cond ((not (%com-windows-p)) (%com-skip test "not Windows") nil)
        (t (let ((ready '()) (why '()))
             (dolist (id progids)
               (handler-case (progn (dotnet:new id) (push id ready))
                 (error (e) (push (format nil "~A (~A)" id (%com-error-line e)) why))))
             (when (and (null ready) why)
               (%com-skip test (format nil "none of these COM classes can be created: ~{~A~^; ~}" (reverse why))))
             (reverse ready)))))

(defun %com-wmi-ready-p (test)
  "True when this is Windows and WMI answers: the locator connects to root\\default and gives StdRegProv."
  (cond ((not (%com-windows-p)) (%com-skip test "not Windows"))
        (t (handler-case
               (progn (dotnet:invoke (dotnet:invoke (dotnet:new "WbemScripting.SWbemLocator")
                                                    "ConnectServer" "." "root\\default")
                                     "Get" "StdRegProv")
                      t)
             (error (e) (%com-skip test (format nil "WMI is not usable (~A)" (%com-error-line e))))))))
