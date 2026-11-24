;;; A member call no overload accepts says what it was given, which overloads there
;;; are, and the usual reason, instead of the CLR's "Method 'T.M' not found."; and a
;;; Task is printed as the public Task type with its state, not as the compiler's
;;; state-machine box.

(defun %dnf-message (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (e) (princ-to-string e))))

(deftest-compiled-only dotnet-member-not-found-wording.names-argument-types-and-overloads
  (let ((m (%dnf-message (lambda () (dotnet:static "System.Math" "Sqrt" "x" "y")))))
    (and m (search "No overload of System.Math.Sqrt takes (string, string)" m)
         (search "Sqrt(Double d) -> Double" m) t))
  t)

(deftest-compiled-only dotnet-member-not-found-wording.out-parameter-points-to-call-out
  (let ((m (%dnf-message
            (lambda ()
              (dotnet:invoke (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.Dictionary"
                                                                   (list "System.String" "System.Int32")))
                             "TryGetValue" "a")))))
    (and m (search "out Int32 value" m) (search "dotnet:call-out" m) t))
  t)

(deftest-compiled-only dotnet-member-not-found-wording.generic-and-list-and-span-hints
  (list (let ((m (%dnf-message (lambda () (dotnet:static "System.Text.Json.JsonSerializer" "Serialize" (list 1 2 3))))))
          (and m (search "dotnet:static-generic" m) (search "Lisp list is not a .NET collection" m) t))
        (let ((m (%dnf-message (lambda () (dotnet:static "System.MemoryExtensions" "AsSpan" (dotnet:new-array "System.Byte" 1 2))))))
          (and m (search "ref struct" m) t)))
  (t t))

(deftest-compiled-only dotnet-member-not-found-wording.plain-task-has-no-result
  (let ((m (%dnf-message (lambda ()
                           (dotnet:invoke (dotnet:static "System.Threading.Tasks.Task" "Run" (lambda () 42))
                                          "Result")))))
    (and m (search "plain Task" m) (search "dotnet:static-generic" m) t))
  t)

(deftest-compiled-only dotnet-member-not-found-wording.misspelt-name-lists-similar
  (let ((m (%dnf-message (lambda () (dotnet:invoke "abc" "tolower2")))))
    (and m (search "has no method of that name" m) (search "Similar members: ToLower" m) t))
  t)

(deftest-compiled-only dotnet-member-not-found-wording.task-prints-as-the-public-type
  (let* ((task (dotnet:static-generic "System.Threading.Tasks.Task" "Run" '("System.Int32") (lambda () 42)))
         (text (princ-to-string task)))
    (dotnet:await task)
    (list (and (search "Task`1[System.Int32]" text) (search "dotnet:await" text) t)
          (not (search "StateMachineBox" text))
          (and (search "RanToCompletion" (princ-to-string task)) t)))
  (t t t))
