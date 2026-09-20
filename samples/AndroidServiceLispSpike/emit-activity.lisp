;;; Emit an Android.App.Activity subclass from Common Lisp, the launcher
;;; activity of an app, into a .dll the Android build can generate an Android
;;; Callable Wrapper for.
;;;
;;; This is the Service sample's companion. The mechanism is identical (see
;;; emit-service.lisp for why the emitting desktop process can use the real
;;; Android.App types without a Java runtime); what it adds is the attribute
;;; half. Android.App.ActivityAttribute has a single parameterless constructor,
;;; so everything that matters is a named property: MainLauncher decides whether
;;; the SDK writes a MAIN/LAUNCHER intent-filter into the manifest, and Label
;;; supplies the name the launcher shows. Neither could be written before named
;;; properties reached attribute specs, which is why the Service went first.
;;;
;;; Exported is set explicitly because a component carrying an intent-filter
;;; must declare android:exported from Android 12 (API 31) on, and this app
;;; targets a newer one.
;;;
;;; Run:
;;;   dotnet run -c Release --project runtime/runtime.csproj -- \
;;;     --asm compiler/cil-out.sil samples/AndroidServiceLispSpike/emit-activity.lisp

(require :dotnet-class)

(defparameter *android-pack-root*
  "C:/Program Files/dotnet/packs/Microsoft.Android.Runtime.36.android")

(defparameter *log-tag* "dotcl-activity")

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

(let ((dll (find-mono-android)))
  (format t "~&Mono.Android: ~a~%" dll)
  (dotnet:load-assembly dll))

(format t "~&Android.App.Activity resolves: ~a~%"
        (dotnet:resolve-type "Android.App.Activity"))

;;; Bodies run on the device, with DotCL.Runtime and this Lisp loaded.
(defun activity-on-create (self saved-instance-state)
  ;; The base implementation has to run: the framework raises
  ;; SuperNotCalledException if an activity returns from OnCreate without it.
  (dotnet:call-base self "OnCreate" saved-instance-state)
  (dotnet:static "Android.Util.Log" "Info" *log-tag*
                 "OnCreate reached Common Lisp")
  nil)

(defun activity-on-resume (self)
  (dotnet:call-base self "OnResume")
  (dotnet:static "Android.Util.Log" "Info" *log-tag*
                 "OnResume reached Common Lisp")
  nil)

(dotnet:library ("LispAndroidActivitySpike" :version "1.0.0.0"
                 :path "samples/AndroidServiceLispSpike/out/LispAndroidActivitySpike.dll")
  (:class "LispSpike.LispMainActivity" ("Android.App.Activity")
    (:doc "The launcher activity, written in Common Lisp.")
    (:attributes
     ("Android.App.ActivityAttribute"
      :main-launcher t
      :label "Lisp Activity"
      :exported t))
    ;; The ACW activation constructor, as in the Service sample: Java-side
    ;; construction reaches managed code through mono.android.TypeManager.
    (:ctor ((handle "System.IntPtr") (transfer "Android.Runtime.JniHandleOwnership"))
      (:base handle transfer))
    (:ctor ())
    (:methods
     ;; Both of these are protected virtual on Android.App.Activity, unlike the
     ;; Service overrides, which were public. They are emitted public, which
     ;; widens accessibility rather than narrowing it.
     ("OnCreate" ((saved-instance-state "Android.OS.Bundle"))
      :returns "System.Void" :override t
      (activity-on-create self saved-instance-state))
     ("OnResume" () :returns "System.Void" :override t
      (activity-on-resume self)))))

(format t "~&saved: ~a~%"
        (probe-file "samples/AndroidServiceLispSpike/out/LispAndroidActivitySpike.dll"))
