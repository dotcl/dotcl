;;; NIL given where every overload of a member takes a value type that is not a bool or a Nullable is a TYPE-ERROR
;;; (datum NIL), not default(T): (dotnet:static "System.Math" "Abs" nil) used to answer 0. A reference or Nullable
;;; parameter still takes NIL as null, a bool as false, and the nearby correct calls still work.

(defun %dnv-error (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (type-error (e) (list :type-error (type-error-datum e) (type-error-expected-type e)))
    (error (e) (list :other (type-of e)))))

(deftest dotnet-nil-value-type.static-int-overloads
  (%dnv-error (lambda () (dotnet:static "System.Math" "Abs" nil)))
  (:type-error nil (signed-byte 32)))

(deftest dotnet-nil-value-type.static-second-argument
  (%dnv-error (lambda () (dotnet:static "System.Math" "Max" 1 nil)))
  (:type-error nil (signed-byte 32)))

(deftest dotnet-nil-value-type.constructor
  (%dnv-error (lambda () (dotnet:new "System.Random" nil)))
  (:type-error nil (signed-byte 32)))

;; a struct that is not a number keeps default(T): nil is "no CancellationToken"
(deftest dotnet-nil-value-type.struct-keeps-its-default
  (let ((ms (dotnet:new "System.IO.MemoryStream" 8)))
    (dotnet:invoke ms "WriteAsync" (dotnet:make-array "System.Byte" 1) nil)
    t)
  t)

;; the nearby correct inputs
(deftest dotnet-nil-value-type.zero-and-other-values
  (list (dotnet:static "System.Math" "Abs" 0)
        (dotnet:static "System.Math" "Abs" 0.0d0)
        (dotnet:static "System.Math" "Abs" -5)
        (dotnet:static "System.Math" "Max" 1 2)
        (dotnet:static "System.Math" "Max" 1.5d0 2.5d0))
  (0 0.0d0 5 2 2.5d0))

(deftest dotnet-nil-value-type.nil-where-an-overload-takes-null
  ;; the string overload of StringBuilder, a string / object overload of Convert and Insert
  (list (dotnet:invoke (dotnet:new "System.Text.StringBuilder" nil) "ToString")
        (dotnet:static "System.Convert" "ToInt32" nil)
        (dotnet:invoke (dotnet:invoke (dotnet:new "System.Text.StringBuilder") "Insert" 0 nil) "ToString")
        (dotnet:static "System.String" "IsNullOrEmpty" nil))
  ("" 0 "" t))

(deftest dotnet-nil-value-type.bool-takes-nil-as-false
  (let ((thread (dotnet:static "System.Threading.Thread" "get_CurrentThread")))
    (dotnet:%set-invoke thread "IsBackground" nil)
    (dotnet:invoke thread "get_IsBackground"))
  nil)

(deftest dotnet-nil-value-type.dotnet-null-marker
  (dotnet:static "System.String" "IsNullOrEmpty" (dotnet:null))
  t)
