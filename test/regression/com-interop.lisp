;;; COM objects that ship with Windows. Each test runs only on Windows and
;;; answers its expected value elsewhere.

(defun %windows-p () (dotnet:static "System.OperatingSystem" "IsWindows"))

;;; A COM object cast to a vtable-only interface: IConnectionPoint has no
;;; IDispatch, so dotnet:invoke has to call through the cast interface. An
;;; unknown cookie answers CONNECT_E_NOCONNECTION (0x80040200) from the vtable
;;; call itself.
(deftest com-invoke-through-cast-vtable-interface
  (if (%windows-p)
      (let* ((sink (dotnet:new "WbemScripting.SWbemSink"))
             (cp (nth-value 2 (dotnet:call-out
                               (dotnet:cast sink "System.Runtime.InteropServices.ComTypes.IConnectionPointContainer")
                               "FindConnectionPoint"
                               (dotnet:new "System.Guid" "75718ca0-f029-11d1-a1ac-00c04fb6c223")))))
        (handler-case
            (progn (dotnet:invoke (dotnet:cast cp "System.Runtime.InteropServices.ComTypes.IConnectionPoint")
                                  "Unadvise" 12345)
                   :no-error)
          (error (e)
            (let ((x (dotnet:exception-object e)))
              (and x (logand #xffffffff (dotnet:invoke x "HResult")))))))
      #x80040200)
  #x80040200)

;;; IDispatch answers an unknown name with a COMException of its own (not
;;; wrapped in TargetInvocationException). It is reported like any other failed
;;; .NET call: an ERROR naming the member, with the COMException behind it.
(deftest com-unknown-member-names-the-member
  (if (%windows-p)
      (handler-case (progn (dotnet:invoke (dotnet:new "Scripting.FileSystemObject") "NoSuchMember") :no-error)
        (error (e)
          (list (type-of e)
                (search "__ComObject.NoSuchMember" (princ-to-string e))
                (dotnet:exception-typep e "System.Runtime.InteropServices.COMException")
                (logand #xffffffff (dotnet:invoke (dotnet:exception-object e) "HResult")))))
      (list 'error 14 t #x80020006))
  (error 14 t #x80020006))
