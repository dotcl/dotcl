;;; Emit an Android.App.Service subclass from Common Lisp into a .dll that the
;;; Android build's Java Callable Wrapper (ACW) generator can see.
;;;
;;; The emitting process is a desktop (net10.0) dotcl. It loads the Android
;;; runtime pack's implementation Mono.Android.dll purely for metadata: the base
;;; type, the activation constructor and the virtual slots to override. That
;;; assembly is not a reference assembly, so CoreCLR loads it for execution and
;;; Type/GetConstructor/GetMethod all answer. No Java runtime is needed because
;;; no Mono.Android code is ever run here.
;;;
;;; Because the base type comes from the real Mono.Android, the saved .dll's
;;; assembly reference already carries the correct identity
;;; (Mono.Android, 0.0.0.0, PublicKeyToken=84e04ff9cfb79065), which is the one
;;; thing the Android SDK looks for when deciding whether an assembly may
;;; contribute Java types.
;;;
;;; Run:
;;;   dotnet run -c Release --project runtime/runtime.csproj -- \
;;;     --asm compiler/cil-out.sil samples/AndroidServiceLispSpike/emit-service.lisp

(require :dotnet-class)

(defparameter *android-pack-root*
  "C:/Program Files/dotnet/packs/Microsoft.Android.Runtime.36.android")

(defun find-mono-android ()
  "Newest Mono.Android.dll under the installed Android runtime pack."
  (let ((candidates
          (sort (remove-if-not
                 (lambda (p) (probe-file p))
                 (mapcar (lambda (v)
                           (format nil "~a/~a/runtimes/android/lib/net10.0/Mono.Android.dll"
                                   *android-pack-root* v))
                         (list "36.1.69" "36.1.53" "36.1.43")))
                #'string>)))
    (or (first candidates)
        (error "no Mono.Android.dll found under ~a" *android-pack-root*))))

(defparameter *out-dll* "samples/AndroidServiceLispSpike/out/LispAndroidSpike.dll")

(let ((dll (find-mono-android)))
  (format t "~&Mono.Android: ~a~%" dll)
  (dotnet:load-assembly dll))

(format t "~&Android.App.Service resolves: ~a~%"
        (dotnet:resolve-type "Android.App.Service"))

;;; The Lisp bodies below are what a real service would carry. In the saved
;;; facade they become DispatchLispMethod call sites, so the bodies run on the
;;; device (with DotCL.Runtime and this Lisp loaded), not here.
(defun service-on-create (self)
  (declare (ignorable self))
  (format t "~&[lisp] OnCreate~%")
  nil)

(defun service-on-start-command (self intent flags start-id)
  (declare (ignorable self intent flags))
  (format t "~&[lisp] OnStartCommand id=~a~%" start-id)
  ;; StartCommandResult.Sticky
  1)

(dotnet:library ("LispAndroidSpike" :version "1.0.0.0" :path "samples/AndroidServiceLispSpike/out/LispAndroidSpike.dll")
  (:class "LispSpike.LispEchoService" ("Android.App.Service")
    (:doc "A Service written in Common Lisp. Its overrides dispatch to Lisp.")
    (:attributes ("Android.App.ServiceAttribute"))
    ;; The ACW activation constructor: Java-side construction reaches managed
    ;; code through mono.android.TypeManager, which needs exactly this pair and
    ;; a chain to the base constructor (protected on Android.App.Service).
    (:ctor ((handle "System.IntPtr") (transfer "Android.Runtime.JniHandleOwnership"))
      (:base handle transfer))
    (:ctor ())
    (:methods
     ("OnCreate" () :returns "System.Void" :override t
      (service-on-create self))
     ;; OnBind is abstract on Android.App.Service, so a concrete subclass must
     ;; supply it even when the service is not bindable.
     ("OnBind" ((intent "Android.Content.Intent")) :returns "Android.OS.IBinder" :override t
      (declare (ignorable intent))
      nil)
     ("OnStartCommand" ((intent "Android.Content.Intent")
                        (flags "Android.App.StartCommandFlags")
                        (start-id "System.Int32"))
      :returns "Android.App.StartCommandResult" :override t
      (service-on-start-command self intent flags start-id))
     ("OnDestroy" () :returns "System.Void" :override t
      (format t "~&[lisp] OnDestroy~%")
      nil))))

(format t "~&saved: ~a~%" (probe-file *out-dll*))
