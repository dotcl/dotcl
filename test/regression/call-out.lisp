;;; Regression tests for D894 — dotnet:call-out: ref/out parameter support (#190)

;;; Int32.TryParse(string, out int) — static, successful parse
(deftest d894-try-parse-success
  (multiple-value-bind (ok n)
      (dotnet:call-out "System.Int32" "TryParse" "42")
    (list ok n))
  (t 42))

;;; Int32.TryParse — failure case returns nil + 0
(deftest d894-try-parse-failure
  (multiple-value-bind (ok n)
      (dotnet:call-out "System.Int32" "TryParse" "not-a-number")
    (list (not ok) n))
  (t 0))

;;; Double.TryParse
(deftest d894-double-try-parse
  (multiple-value-bind (ok n)
      (dotnet:call-out "System.Double" "TryParse" "3.14")
    (and ok (< (abs (- n 3.14)) 0.001)))
  t)

;;; Dictionary.TryGetValue — instance method with out param
(deftest d894-dict-try-get-value
  (let ((d (dotnet:new "System.Collections.Generic.Dictionary`2[System.String,System.Int32]")))
    (setf (dotnet:ref d "answer") 42)
    (multiple-value-bind (found val)
        (dotnet:call-out d "TryGetValue" "answer")
      (list found val)))
  (t 42))

;;; TryGetValue — key absent
(deftest d894-dict-try-get-missing
  (let ((d (dotnet:new "System.Collections.Generic.Dictionary`2[System.String,System.Int32]")))
    (multiple-value-bind (found val)
        (dotnet:call-out d "TryGetValue" "missing")
      (list (not found) val)))
  (t 0))
