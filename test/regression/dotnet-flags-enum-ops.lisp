;;; LOGIOR, LOGAND, LOGXOR, LOGTEST, LOGANDC1 and LOGANDC2 on [Flags] enum
;;; values return that enum type; a non-Flags enum and a mix of two enum types
;;; stay TYPE-ERRORs.

(defun flags-enum-fa (name) (dotnet:static "System.IO.FileAttributes" name))
(defun flags-enum-rx (name) (dotnet:static "System.Text.RegularExpressions.RegexOptions" name))
(defun flags-enum-ek (name) (dotnet:static "System.Diagnostics.Tracing.EventKeywords" name))
(defun flags-enum-sa (name) (dotnet:static "System.Reflection.Metadata.SignatureAttributes" name))
(defun flags-enum-ul (name) (dotnet:static "DotCL.TestSupport.ULongFlags" name))

(defun flags-enum-int64 (value)
  (dotnet:static "System.Convert" "ToInt64" value))

(defun flags-enum-error (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (error (condition) (format nil "~a" condition))))

(deftest dotnet-flags-enum.logior
  (let ((value (logior (flags-enum-fa "Hidden") (flags-enum-fa "ReadOnly"))))
    (list (dotnet:invoke value "ToString")
          (flags-enum-int64 value)
          (notnot (dotnet:is-instance-of value "System.IO.FileAttributes"))))
  ("ReadOnly, Hidden" 3 t))

(deftest dotnet-flags-enum.logand
  (let ((value (logand (flags-enum-fa "Hidden") (flags-enum-fa "ReadOnly"))))
    (list (dotnet:invoke value "ToString") (flags-enum-int64 value)))
  ("None" 0))

(deftest dotnet-flags-enum.logxor
  (let ((value (logxor (flags-enum-fa "Hidden") (flags-enum-fa "ReadOnly"))))
    (list (dotnet:invoke value "ToString") (flags-enum-int64 value)))
  ("ReadOnly, Hidden" 3))

(deftest dotnet-flags-enum.logtest
  (let ((both (logior (flags-enum-fa "Hidden") (flags-enum-fa "ReadOnly"))))
    (list (logtest (flags-enum-fa "Hidden") both)
          (logtest (flags-enum-fa "Archive") both)))
  (t nil))

(deftest dotnet-flags-enum.logandc
  (let ((both (logior (flags-enum-fa "Hidden") (flags-enum-fa "ReadOnly"))))
    (list (dotnet:invoke (logandc2 both (flags-enum-fa "Hidden")) "ToString")
          (dotnet:invoke (logandc1 (flags-enum-fa "Hidden") both) "ToString")))
  ("ReadOnly" "ReadOnly"))

(deftest dotnet-flags-enum.enum-and-integer
  (let ((both (logior (flags-enum-fa "Hidden") (flags-enum-fa "ReadOnly"))))
    (list (dotnet:invoke (logior (flags-enum-fa "Hidden") 4) "ToString")
          (flags-enum-int64 (logand both 3))
          (dotnet:invoke (logxor (flags-enum-fa "Hidden") 4) "ToString")
          (logtest (flags-enum-fa "Hidden") 2)
          (dotnet:invoke (logandc2 both 1) "ToString")))
  ("Hidden, System" 3 "Hidden, System" t "Hidden"))

(deftest dotnet-flags-enum.nary
  (let ((value (apply #'logior
                      (list (flags-enum-fa "Hidden") (flags-enum-fa "ReadOnly")
                            (flags-enum-fa "Archive") (flags-enum-fa "System")
                            (flags-enum-fa "Temporary")))))
    (list (dotnet:invoke value "ToString") (flags-enum-int64 value)))
  ("ReadOnly, Hidden, System, Archive, Temporary" 295))

(deftest dotnet-flags-enum.has-flag
  (let ((both (logior (flags-enum-fa "Hidden") (flags-enum-fa "ReadOnly"))))
    (list (dotnet:invoke both "HasFlag" (flags-enum-fa "ReadOnly"))
          (dotnet:invoke both "HasFlag" (flags-enum-fa "Archive"))))
  (t nil))

(deftest dotnet-flags-enum.property
  (let ((watcher (dotnet:new "System.IO.FileSystemWatcher")))
    (setf (dotnet:invoke watcher "NotifyFilter")
          (logior (dotnet:static "System.IO.NotifyFilters" "FileName")
                  (dotnet:static "System.IO.NotifyFilters" "LastWrite")))
    (dotnet:invoke (dotnet:invoke watcher "get_NotifyFilter") "ToString"))
  "FileName, LastWrite")

(deftest dotnet-flags-enum.regex-constructor
  (let ((regex (dotnet:new "System.Text.RegularExpressions.Regex" "abc"
                           (logior (flags-enum-rx "IgnoreCase")
                                   (flags-enum-rx "Multiline")))))
    (dotnet:invoke regex "IsMatch" "ABC"))
  t)

(deftest dotnet-flags-enum.regex-static-method
  (dotnet:static "System.Text.RegularExpressions.Regex" "IsMatch" "a
b" "^b" (logior (flags-enum-rx "IgnoreCase") (flags-enum-rx "Multiline")))
  t)

(deftest dotnet-flags-enum.non-flags-errors
  (let ((message (flags-enum-error
                   (lambda () (logior (dotnet:static "System.DayOfWeek" "Monday")
                                      (dotnet:static "System.DayOfWeek" "Tuesday"))))))
    (list (and (search "[Flags]" message) t)
          (and (search "System.DayOfWeek" message) t)))
  (t t))

(deftest dotnet-flags-enum.different-types-error
  (let ((message (flags-enum-error
                   (lambda () (logior (flags-enum-fa "Hidden")
                                      (flags-enum-rx "IgnoreCase"))))))
    (list (and (search "System.IO.FileAttributes" message) t)
          (and (search "System.Text.RegularExpressions.RegexOptions" message) t)))
  (t t))

(deftest dotnet-flags-enum.integer-round-trip
  (list (flags-enum-int64 (flags-enum-fa "Hidden"))
        (dotnet:invoke (dotnet:static "System.Enum" "ToObject"
                                      (dotnet:resolve-type "System.IO.FileAttributes") 3)
                       "ToString"))
  (2 "ReadOnly, Hidden"))

(deftest dotnet-flags-enum.integer-to-enum
  (list (dotnet:invoke (dotnet:box 1048576 "System.IO.FileAttributes") "ToString" "D")
        (let ((watcher (dotnet:new "System.IO.FileSystemWatcher")))
          (setf (dotnet:invoke watcher "NotifyFilter") 3)
          (dotnet:invoke (dotnet:invoke watcher "get_NotifyFilter") "ToString")))
  ("1048576" "FileName, DirectoryName"))

(deftest dotnet-flags-enum.underlying-long
  (list (flags-enum-int64 (logior (flags-enum-ek "AuditFailure")
                                  (flags-enum-ek "AuditSuccess")))
        (flags-enum-int64 (logand (flags-enum-ek "All")
                                  (flags-enum-ek "AuditFailure"))))
  (13510798882111488 4503599627370496))

(deftest dotnet-flags-enum.underlying-byte
  (list (flags-enum-int64 (logior (flags-enum-sa "Generic")
                                  (flags-enum-sa "Instance")))
        (flags-enum-int64 (logand (logior (flags-enum-sa "Generic")
                                          (flags-enum-sa "Instance"))
                                  (flags-enum-sa "Instance"))))
  (48 32))

(deftest dotnet-flags-enum.underlying-ulong
  (let ((value (logior (flags-enum-ul "High") (flags-enum-ul "Low"))))
    (list (dotnet:static "System.Convert" "ToUInt64" value)
          (dotnet:static "System.Convert" "ToUInt64"
                         (logandc2 value (flags-enum-ul "High")))))
  (9223372036854775809 1))

(deftest dotnet-flags-enum.unary
  (list (dotnet:invoke (logior (flags-enum-fa "Hidden")) "ToString")
        (dotnet:invoke (logand (flags-enum-fa "Hidden")) "ToString")
        (dotnet:invoke (logxor (flags-enum-fa "Hidden")) "ToString"))
  ("Hidden" "Hidden" "Hidden"))

(deftest dotnet-flags-enum.fixnum-path-unchanged
  (list (logior 1 2) (logand 7 3) (logxor 1 3) (logtest 2 3) (lognot 0))
  (3 3 2 t -1))
