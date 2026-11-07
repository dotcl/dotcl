;;; Trying the overloads of a .NET call in turn is not an error: no handler hears of
;;; the candidates that do not fit, and the call that fits returns its value. A call
;;; no overload fits still signals (once), and can be handled.

(deftest dotnet-overload-trial-quiet.no-handler-runs
  (let ((heard 0))
    (handler-bind ((error (lambda (c) (declare (ignore c)) (incf heard))))
      (dotnet:static "System.Text.Json.JsonDocument" "Parse" "{}"))
    heard)
  0)

(deftest dotnet-overload-trial-quiet.value
  (typep (dotnet:static "System.Text.Json.JsonDocument" "Parse" "{}") 't)
  t)

(deftest dotnet-overload-trial-quiet.no-fitting-overload-still-signals
  (let ((heard 0))
    (list (handler-case
              (handler-bind ((error (lambda (c) (declare (ignore c)) (incf heard))))
                (dotnet:static "System.Math" "Abs" "x"))
            (error () :error))
          (>= heard 1)))
  (:error t))
