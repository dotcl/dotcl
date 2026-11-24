;;; An enum argument binds to the Object overload, as C# does.

(defparameter *enum-friday* (dotnet:static "System.DayOfWeek" "Friday"))

(defparameter *enum-flags*
  (logior (dotnet:static "System.IO.FileAttributes" "Hidden")
          (dotnet:static "System.IO.FileAttributes" "ReadOnly")))

(defun capture-console-output (thunk)
  (let* ((orig (dotnet:static "System.Console" "Out"))
         (sw (dotnet:new "System.IO.StringWriter")))
    (unwind-protect
        (progn
          (dotnet:static "System.Console" "SetOut" sw)
          (funcall thunk)
          (dotnet:invoke sw "ToString"))
      (dotnet:static "System.Console" "SetOut" orig))))

(defun enum-newline (text)
  "TEXT followed by the line end of this platform: AppendLine and WriteLine write Environment.NewLine."
  (concatenate 'string text (dotnet:static "System.Environment" "NewLine")))

(defun enum-fixed-results (value)
  (list
   (let ((sb (dotnet:new "System.Text.StringBuilder")))
     (dotnet:invoke sb "Append" value)
     (dotnet:invoke sb "ToString"))
   (let ((sb (dotnet:new "System.Text.StringBuilder")))
     (dotnet:invoke sb "Insert" 0 value)
     (dotnet:invoke sb "ToString"))
   (let ((sb (dotnet:new "System.Text.StringBuilder")))
     (dotnet:invoke sb "AppendLine" value)
     (dotnet:invoke sb "ToString"))
   (capture-console-output
    (lambda () (dotnet:static "System.Console" "WriteLine" value)))
   (capture-console-output
    (lambda () (dotnet:static "System.Console" "Write" value)))
   (let ((tw (dotnet:new "System.IO.StringWriter")))
     (dotnet:invoke tw "Write" value)
     (dotnet:invoke tw "ToString"))
   (let ((tw (dotnet:new "System.IO.StringWriter")))
     (dotnet:invoke tw "WriteLine" value)
     (dotnet:invoke tw "ToString"))
   (dotnet:static "System.Convert" "ToString" value)))

(deftest enum-overload-day-of-week-object-forms
  (enum-fixed-results *enum-friday*)
  ("Friday" "Friday" #.(enum-newline "Friday") #.(enum-newline "Friday")
        "Friday" "Friday" #.(enum-newline "Friday") "Friday"))

(deftest enum-overload-string-comparison-object-forms
  (enum-fixed-results (dotnet:static "System.StringComparison" "Ordinal"))
  ("Ordinal" "Ordinal" #.(enum-newline "Ordinal") #.(enum-newline "Ordinal")
        "Ordinal" "Ordinal" #.(enum-newline "Ordinal") "Ordinal"))

(deftest enum-overload-file-mode-object-forms
  (enum-fixed-results (dotnet:static "System.IO.FileMode" "Append"))
  ("Append" "Append" #.(enum-newline "Append") #.(enum-newline "Append")
        "Append" "Append" #.(enum-newline "Append") "Append"))

(deftest enum-overload-flags-object-forms
  (enum-fixed-results *enum-flags*)
  ("ReadOnly, Hidden" "ReadOnly, Hidden"
        #.(enum-newline "ReadOnly, Hidden")
        #.(enum-newline "ReadOnly, Hidden")
        "ReadOnly, Hidden" "ReadOnly, Hidden"
        #.(enum-newline "ReadOnly, Hidden") "ReadOnly, Hidden"))

(deftest enum-overload-typed-direct-inline
  (list
   (dotnet:static "System.Convert" "ToString"
                  (dotnet:static "System.StringComparison" "Ordinal"))
   (let ((sb (dotnet:new "System.Text.StringBuilder"))
         (e (dotnet:static "System.DayOfWeek" "Friday")))
     (dotnet:invoke sb "Append" e)
     (dotnet:invoke sb "ToString")))
  ("Ordinal" "Friday"))

(deftest enum-overload-numeric-only-parameter
  (char-code
   (char (dotnet:static "System.Char" "ConvertFromUtf32"
                        (dotnet:static "System.DayOfWeek" "Friday"))
         0))
  5)

(deftest enum-overload-numeric-sibling-without-object-parameter
  (dotnet:static "System.Math" "Sign" *enum-friday*)
  1)

(deftest enum-overload-enum-parameter
  (dotnet:invoke *enum-friday* "HasFlag" *enum-friday*)
  t)

(deftest enum-overload-string-concat-format-join
  (list
   (dotnet:static "System.String" "Concat" *enum-friday*)
   (dotnet:static "System.String" "Concat" *enum-friday* *enum-friday*)
   (dotnet:static "System.String" "Concat" "a" *enum-friday* "b")
   (dotnet:static "System.String" "Concat" "a" *enum-friday* "b"
                  *enum-friday*)
   (dotnet:static "System.String" "Format" "{0}" *enum-friday*)
   (dotnet:static "System.String" "Join" ","
                  (list *enum-friday* *enum-friday*))
   (let ((sb (dotnet:new "System.Text.StringBuilder")))
     (dotnet:invoke sb "AppendFormat" "{0}" *enum-friday*)
     (dotnet:invoke sb "ToString")))
  ("Friday" "FridayFriday" "aFridayb" "aFridaybFriday" "Friday"
   "Friday,Friday" "Friday"))

(deftest enum-overload-flags-format
  (list
   (dotnet:static "System.Enum" "Format"
                  (dotnet:resolve-type "System.IO.FileAttributes")
                  *enum-flags* "G")
   (dotnet:static "System.Enum" "Format"
                  (dotnet:resolve-type "System.IO.FileAttributes")
                  *enum-flags* "D")
   (dotnet:static "System.Enum" "Format"
                  (dotnet:resolve-type "System.IO.FileAttributes")
                  *enum-flags* "X"))
  ("ReadOnly, Hidden" "3" "00000003"))
