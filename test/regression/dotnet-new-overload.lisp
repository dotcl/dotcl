;;; DOTNET:NEW picks a constructor the arguments convert to.

(defun dnew-octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

;;; MemoryStream(byte[]) for a vector, MemoryStream(int) for an integer. The
;;; stream is over the vector's own storage.
(deftest dnew-memory-stream-over-octets
  (let* ((b (dnew-octets 1 2 3))
         (s (dotnet:new "System.IO.MemoryStream" b)))
    (dotnet:invoke s "WriteByte" 9)
    (list (dotnet:invoke s "get_Length")
          (coerce b 'list)
          (dotnet:invoke (dotnet:new "System.IO.MemoryStream" '(1 2)) "get_Length")
          (dotnet:invoke (dotnet:new "System.IO.MemoryStream" 16) "get_Capacity")))
  (3 (9 2 3) 2 16))

(deftest dnew-string-and-biginteger-from-vectors
  (list (dotnet:invoke (dotnet:new "System.String" "abc") "ToString")
        (dotnet:invoke (dotnet:new "System.String" #\a 3) "ToString")
        (dotnet:invoke (dotnet:new "System.Numerics.BigInteger" (dnew-octets 1 1)) "ToString"))
  ("abc" "aaa" "257"))

;;; A vector with a fill pointer is not copied into a constructor that keeps the array.
(deftest dnew-fill-pointer-vector-signals
  (handler-case
      (dotnet:new "System.IO.MemoryStream"
                  (make-array 3 :element-type '(unsigned-byte 8) :fill-pointer 2))
    (type-error () :type-error))
  :type-error)
