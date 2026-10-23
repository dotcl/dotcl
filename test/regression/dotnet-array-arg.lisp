;;; A Lisp vector passed to a .NET T[] (or System.Array) parameter.
;;;
;;; A simple vector whose storage is that array type is passed as the array
;;; itself, so what the method writes lands in the vector. A vector with such
;;; storage but a fill pointer or a displacement signals instead of being
;;; copied (the writes would be lost). Lists and vectors of another element
;;; type are still copied in.

(defun darr-octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defun darr-stream (&rest bytes)
  (dotnet:new "System.IO.MemoryStream"
              (dotnet:static "System.Convert" "FromHexString"
                             (format nil "~{~2,'0X~}" bytes))))

;;; Stream.Read(byte[], int, int) fills the Lisp vector.
(deftest darr-stream-read-byte-array
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8) :initial-element 9)))
    (list (dotnet:invoke (darr-stream 1 2 3) "Read" buf 0 3) (coerce buf 'list)))
  (3 (1 2 3 9)))

;;; System.Array parameters: Array.Copy and Buffer.BlockCopy.
(deftest darr-array-copy-and-block-copy
  (let ((dst1 (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0))
        (dst2 (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotnet:static "System.Array" "Copy" (darr-octets 1 2 3) dst1 3)
    (dotnet:static "System.Buffer" "BlockCopy" (darr-octets 4 5 6) 0 dst2 1 2)
    (list (coerce dst1 'list) (coerce dst2 'list)))
  ((1 2 3) (0 4 5)))

;;; TextReader.Read(char[], int, int) into a string, and into an adjustable
;;; character vector without a fill pointer.
(deftest darr-text-reader-read-char-array
  (let ((s (make-string 4 :initial-element #\-))
        (v (make-array 3 :element-type 'character :initial-element #\- :adjustable t)))
    (dotnet:invoke (dotnet:new "System.IO.StringReader" "abc") "Read" s 1 3)
    (dotnet:invoke (dotnet:new "System.IO.StringReader" "xyz") "Read" v 0 2)
    (list s (coerce v 'string)))
  ("-abc" "xy-"))

;;; List<double>.CopyTo(double[]) and Array.Reverse(System.Array) on a double vector.
(deftest darr-double-array
  (let ((list (dotnet:new "System.Collections.Generic.List`1[System.Double]"))
        (d (make-array 3 :element-type 'double-float :initial-element 0d0)))
    (dotnet:invoke list "Add" 1.5d0)
    (dotnet:invoke list "Add" 2.5d0)
    (dotnet:invoke list "CopyTo" d)
    (let ((after-copy (coerce d 'list)))
      (dotnet:static "System.Array" "Reverse" d)
      (list after-copy (coerce d 'list))))
  ((1.5d0 2.5d0 0d0) (0d0 2.5d0 1.5d0)))

;;; Fill pointer and displacement: an error, not a silent copy.
(deftest darr-non-simple-signals
  (let ((fp (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 2 :initial-element 0))
        (window (make-array 2 :element-type '(unsigned-byte 8)
                              :displaced-to (darr-octets 0 0 0 0) :displaced-index-offset 1)))
    (list (handler-case (dotnet:invoke (darr-stream 1 2) "Read" fp 0 2)
            (type-error () :type-error))
          (handler-case (dotnet:invoke (darr-stream 1 2) "Read" window 0 2)
            (type-error () :type-error))))
  (:type-error :type-error))

;;; Representations that are not the array still go in as copies.
(deftest darr-copies-still-accepted
  (list (dotnet:static "System.Convert" "ToHexString" '(1 2))
        (dotnet:static "System.Convert" "ToHexString" (vector 3 4))
        (dotnet:static "System.Convert" "ToHexString"
                       (make-array 2 :element-type '(unsigned-byte 4) :initial-element 15)))
  ("0102" "0304" "0F0F"))

;;; The typed direct call (receiver and argument types known at compile time).
(defun darr-direct-next-bytes (buf)
  (let ((r (dotnet:new "System.Random" 1)))
    (dotnet:invoke (the (dotnet "System.Random") r) "NextBytes"
                   (the (dotnet "System.Byte[]") buf))))

(deftest darr-direct-call
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0))
        (fp (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 4 :initial-element 0)))
    (darr-direct-next-bytes buf)
    (list (notevery #'zerop buf)
          ;; type-error on the direct call; the emit-free build has no direct call
          ;; and reaches NextBytes(Span<byte>), which needs the emitting runtime.
          (handler-case (darr-direct-next-bytes fp) (error () :error))))
  (t :error))
