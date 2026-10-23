;;; Span<T> / ReadOnlySpan<T> / Memory<T> / ReadOnlyMemory<T> arguments and results.
;;;
;;; A Lisp vector with numeric or character storage is passed as a view of that
;;; storage, so a method writing into a Span writes into the vector. A Span
;;; cannot be boxed, so those calls go through an emitted stub: on an emit-free
;;; build they signal an error instead. Memory<T> needs no stub.

(defun dspan-octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defun dspan-utf8 () (dotnet:static "System.Text.Encoding" "UTF8"))

(defun dspan-stream (string)
  (dotnet:new "System.IO.MemoryStream" (dotnet:invoke (dspan-utf8) "GetBytes" string)))

;;; Encoding.GetBytes(ReadOnlySpan<char>, Span<byte>): the bytes land in the Lisp vector.
(deftest-emitting-only dspan-encode-into-octets
  (let ((buf (make-array 5 :element-type '(unsigned-byte 8) :initial-element 0)))
    (list (dotnet:invoke (dspan-utf8) "GetBytes" "abc" buf) (coerce buf 'list)))
  (3 (97 98 99 0 0)))

;;; Stream.Read(Span<byte>), the only one-argument Read.
(deftest-emitting-only dspan-stream-read
  (let ((buf (make-array 2 :element-type '(unsigned-byte 8))))
    (list (dotnet:invoke (dspan-stream "xyz") "Read" buf) (coerce buf 'list)))
  (2 (120 121)))

;;; The span is the vector's active part: a fill pointer and a displacement both count.
(deftest-emitting-only dspan-fill-pointer-and-displaced
  (let* ((base (dspan-octets 0 1 2 3 4 5))
         (window (make-array 3 :element-type '(unsigned-byte 8)
                               :displaced-to base :displaced-index-offset 2))
         (fp (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 2
                           :initial-contents '(65 66 200 200))))
    (list (dotnet:static "System.Convert" "ToHexString" window)
          (dotnet:static "System.Text.Ascii" "IsValid" fp)
          (dotnet:static "System.Buffers.Binary.BinaryPrimitives" "ReadInt32LittleEndian"
                         (dspan-octets 1 1 0 0))))
  ("020304" t 257))

;;; call-out picks the overload the arguments fit: ReadOnlySpan<byte> for an octet
;;; vector, string for a string. Its out parameters still come back as values.
(deftest-emitting-only dspan-call-out-overloads
  (list (multiple-value-list (dotnet:call-out "System.Int32" "TryParse" (dspan-octets 52 50)))
        (multiple-value-list (dotnet:call-out "System.Int32" "TryParse" "17")))
  ((t 42) (t 17)))

(deftest-emitting-only dspan-call-out-writes-span
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 9)))
    (multiple-value-bind (ok n) (dotnet:call-out "System.Convert" "TryFromBase64String" "AQID" buf)
      (list ok n (coerce buf 'list))))
  (t 3 (1 2 3 9)))

;;; In place on double-float and character storage, through a generic method.
(deftest-emitting-only dspan-generic-in-place
  (let ((d (make-array 3 :element-type 'double-float :initial-contents '(1d0 2d0 3d0)))
        (s (make-array 4 :element-type 'character :initial-contents "abcd" :fill-pointer 3)))
    (dotnet:static-generic "System.MemoryExtensions" "Reverse" '("System.Double") d)
    (dotnet:static-generic "System.MemoryExtensions" "Reverse" '("System.Char") s)
    (list (coerce d 'list) s (dotnet:static-generic "System.MemoryExtensions" "IndexOf" '("System.Char") "hello" #\l)))
  ((3d0 2d0 1d0) "cba" 2))

;;; A read-only span may be a copy: a list, or a vector of another element type.
(deftest-emitting-only dspan-read-only-copy
  (list (dotnet:static-generic "System.MemoryExtensions" "IndexOf" '("System.Int64") '(5 6 7) 7)
        (dotnet:static-generic "System.MemoryExtensions" "IndexOf" '("System.Int32") (dspan-octets 5 6 7) 6))
  (2 1))

;;; A writable one may not: the method would write into a copy nobody sees.
(deftest-emitting-only dspan-writable-needs-storage
  (handler-case
      (progn (dotnet:static-generic "System.MemoryExtensions" "Reverse" '("System.Int64") (list 1 2 3))
             :no-error)
    (error () :error))
  :error)

;;; A returned span comes back as a fresh Lisp vector of the element type.
(deftest-emitting-only dspan-results
  (let ((octets (dotnet:invoke (dotnet:new "System.Memory`1[System.Byte]"
                                           (dotnet:make-array "System.Byte" 3))
                               "get_Span")))
    (list (dotnet:static "System.MemoryExtensions" "AsSpan" "abc")
          (dotnet:static "System.MemoryExtensions" "Trim" "  hi  ")
          (coerce octets 'list)
          (equal (array-element-type octets) '(unsigned-byte 8))))
  ("abc" "hi" (0 0 0) t))

;;; A receiver whose type the compiler knows: the call must not be emitted as a
;;; direct call that boxes the span.
(deftest-emitting-only dspan-typed-receiver
  (let ((m (dotnet:new "System.ReadOnlyMemory`1[System.Char]" (dotnet:make-array "System.Char" 2))))
    (list (length (dotnet:invoke m "get_Span")) (dotnet:invoke m "get_Length")))
  (2 2))

;;; The method's own exception arrives as a Lisp error.
(deftest-emitting-only dspan-method-error
  (handler-case
      (progn (dotnet:invoke (dspan-utf8) "GetBytes" "abcdef" (make-array 2 :element-type '(unsigned-byte 8)))
             :no-error)
    (error () :error))
  :error)

;;; Without an emitter there is no stub, and the call says why.
(unless (emitting-mode-p)
  (deftest dspan-emit-free-error
    (handler-case (progn (dotnet:invoke (dspan-stream "xyz") "Read" (dspan-octets 0 0)) :no-error)
      (error (e) (and (search "Span" (princ-to-string e)) :error)))
    :error))

;;; Memory<byte> needs no stub: ReadAsync fills the vector before its task completes.
(deftest dspan-memory-argument
  (let* ((buf (make-array 3 :element-type '(unsigned-byte 8)))
         (task (dotnet:invoke (dspan-stream "pq") "ReadAsync" buf nil)))
    (list (dotnet:invoke task "get_Result") (coerce buf 'list)))
  (2 (112 113 0)))
