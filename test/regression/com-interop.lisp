;;; COM objects that ship with Windows. Each test runs only where the COM classes it uses can be
;;; created (see com-available.lisp) and answers its expected value elsewhere.

;;; A COM object cast to a vtable-only interface: IConnectionPoint has no
;;; IDispatch, so dotnet:invoke has to call through the cast interface. An
;;; unknown cookie answers CONNECT_E_NOCONNECTION (0x80040200) from the vtable
;;; call itself.
(deftest com-invoke-through-cast-vtable-interface
  (if (%com-ready-p 'com-invoke-through-cast-vtable-interface "WbemScripting.SWbemSink")
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
;;; Any late-bound (IDispatch) COM class shows this, so the test tries each class below that the
;;; machine can create. A class whose type library cannot be loaded answers TYPE_E_CANTLOADLIBRARY
;;; (0x80029C4A) before the name is looked at: that says nothing about unknown names, so such a
;;; class gives no verdict. When no class gives a verdict the test says so on one line and answers
;;; its expected value (NIL, no class misbehaved).
(defparameter *com-unknown-member-classes*
  '("Scripting.FileSystemObject" "Shell.Application" "WScript.Shell"
    "MSXML2.DOMDocument.6.0" "WbemScripting.SWbemLocator"
    ;; Classes of the system's own COM libraries (oleaut32, the task scheduler, the firewall
    ;; policy), which a validation image has although it carries no Windows Script Host.
    "StdFont" "Schedule.Service" "HNetCfg.FwPolicy2"))

(defun %com-unknown-member-result (progid)
  (handler-case (progn (dotnet:invoke (dotnet:new progid) "NoSuchMember") (list progid :no-error))
    (error (e)
      (list progid
            (type-of e)
            (search "__ComObject.NoSuchMember" (princ-to-string e))
            (dotnet:exception-typep e "System.Runtime.InteropServices.COMException")
            (logand #xffffffff (dotnet:invoke (dotnet:exception-object e) "HResult"))))))

(defun %com-no-verdict-p (result)
  (eql (car (last result)) #x80029c4a))

(deftest com-unknown-member-names-the-member
  (let* ((ready (apply #'%com-ready-list 'com-unknown-member-names-the-member *com-unknown-member-classes*))
         (results (mapcar #'%com-unknown-member-result ready))
         (verdicts (remove-if #'%com-no-verdict-p results)))
    (when (and results (null verdicts))
      (%com-skip 'com-unknown-member-names-the-member
                 (format nil "no class gave a verdict (type library not loadable): ~{~A~^, ~}" ready)))
    (when (and results (/= (length results) (length verdicts)))
      (format t "NOTE com-unknown-member-names-the-member: no verdict from ~{~A~^, ~}; verdicts: ~{~A~^, ~}~%"
              (mapcar #'car (remove-if-not #'%com-no-verdict-p results))
              (mapcar #'car verdicts)))
    (remove-if (lambda (r) (equal (cdr r) '(error 14 t #x80020006))) verdicts))
  nil)
