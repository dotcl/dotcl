;;; Manual E2E for the harmony contrib. Run:
;;;   dotnet run --project runtime/runtime.csproj -- --asm compiler/cil-out.sil test/harmony-watch.lisp
;;; (needs network on first run to resolve Lib.Harmony from NuGet)

(require "harmony")

(defvar *log* nil)

(defun call-add ()
  (dotnet:static "DotCL.MethodAdviceBridge" "DemoAdd" 3 4))

(format t "~&before watch: DemoAdd(3,4) = ~a~%" (call-add))

(harmony:watch "DotCL.MethodAdviceBridge" "DemoAdd"
  (lambda (instance args result)
    (declare (ignore instance))
    (push (list :args args :result result) *log*)
    (format t "~&[advice] DemoAdd~s => ~s~%" args result)))

(format t "~&after watch:  DemoAdd(3,4) = ~a~%" (call-add))
(dotnet:static "DotCL.MethodAdviceBridge" "DemoAdd" 10 20)

(harmony:unwatch "DotCL.MethodAdviceBridge" "DemoAdd")
(format t "~&after unwatch: DemoAdd(3,4) = ~a~%" (call-add))

(format t "~&~%captured ~a call(s):~%" (length *log*))
(dolist (e (reverse *log*)) (format t "  ~s~%" e))
(defvar *watch-ok* (= 2 (length *log*)))
(if *watch-ok*
    (format t "~&watch: PASS (advice fired for the 2 calls while watched, not after unwatch)~%")
    (format t "~&watch: FAIL (expected 2 captured calls, got ~a)~%" (length *log*)))

;;; --- patch: rewrite the return value in place ---
(format t "~%--- patch ---~%")
(harmony:patch "DotCL.MethodAdviceBridge" "DemoAdd"
  (lambda (instance args result)
    (declare (ignore instance))
    ;; "fix" the method: return the product instead of whatever it computed.
    (apply #'* args)))

(let ((patched (call-add)))                 ; DemoAdd(3,4): original 7, patched 12
  (format t "~&after patch:   DemoAdd(3,4) = ~a  (original 7)~%" patched)
  (harmony:unpatch "DotCL.MethodAdviceBridge" "DemoAdd")
  (let ((restored (call-add)))
    (format t "~&after unpatch: DemoAdd(3,4) = ~a~%" restored)

    ;;; --- trace: time each call ---
    (format t "~%--- trace ---~%")
    (defvar *tlog* nil)
    (harmony:trace "DotCL.MethodAdviceBridge" "DemoAdd"
      (lambda (instance args result seconds)
        (declare (ignore instance result))
        (push (cons args seconds) *tlog*)
        (format t "~&[trace] DemoAdd~s  ~,4Fms~%" args (* seconds 1000.0))))
    (call-add)
    (dotnet:static "DotCL.MethodAdviceBridge" "DemoAdd" 100 200)
    (harmony:untrace "DotCL.MethodAdviceBridge" "DemoAdd")
    (call-add)                                ; not traced
    (let ((trace-ok (and (= 2 (length *tlog*))
                         (every (lambda (e) (numberp (cdr e))) *tlog*))))
      (format t "~&traced ~a call(s); after untrace not counted~%" (length *tlog*))
      (if (and *watch-ok* (= patched 12) (= restored 7) trace-ok)
          (format t "~&RESULT: PASS (watch + patch 7->12 + unpatch + trace 2 timed calls)~%")
          (format t "~&RESULT: FAIL (watch=~a patched=~a restored=~a trace-ok=~a)~%"
                  *watch-ok* patched restored trace-ok)))))
