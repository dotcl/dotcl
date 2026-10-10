;;; The command-line runtime starts counting calls for tiered compilation after
;;; a short pause in new-method activity, not after the .NET default of 100 ms.
;;; Compiling and loading Lisp code calls new methods continuously, so with the
;;; default the pause never came and the compiler ran unoptimized code for a
;;; whole COMPILE-FILE or QUICKLOAD (generic-cl: about 30% slower).

(deftest tiering-call-counting-delay.configured
  (let ((v (dotnet:static "System.AppContext" "GetData"
                          "System.Runtime.TieredCompilation.CallCountingDelayMs")))
    (and v (<= (parse-integer (princ-to-string v)) 10)))
  t)
