;;; Regression: a file stream opened with :element-type (signed-byte N).
;;; Only (unsigned-byte N) set the element width, so (signed-byte 32) moved a
;;; single byte per READ-BYTE / WRITE-BYTE, and nothing came back negative:
;;; writing -5 stored the one byte #xFB and read it back as 251.
;;; numpy-file-format reads and writes its integer arrays this way.
;;;
;;; Also (unsigned-byte 64): an element with the top bit set was assembled in
;;; a signed 64-bit integer and came back negative.

(defun sbfs-round-trip (type values)
  (let ((path (regression-temp-file "signed-byte-file-stream.bin")))
    (with-open-file (s path :direction :output :element-type type
                            :if-exists :supersede)
      (dolist (v values) (write-byte v s)))
    (list (with-open-file (s path :element-type '(unsigned-byte 8))
            (file-length s))
          (with-open-file (s path :element-type type)
            (loop for b = (read-byte s nil) while b collect b))
          (with-open-file (s path :element-type type)
            (let ((v (make-array (length values))))
              (read-sequence v s)
              (coerce v 'list))))))

(deftest signed-byte-file-stream-8
  (sbfs-round-trip '(signed-byte 8) '(5 -1 -128 127))
  (4 (5 -1 -128 127) (5 -1 -128 127)))

(deftest signed-byte-file-stream-16
  (sbfs-round-trip '(signed-byte 16) '(300 -1 -32768 32767))
  (8 (300 -1 -32768 32767) (300 -1 -32768 32767)))

(deftest signed-byte-file-stream-32
  (sbfs-round-trip '(signed-byte 32) '(1078530011 -5 -2147483648))
  (12 (1078530011 -5 -2147483648) (1078530011 -5 -2147483648)))

(deftest signed-byte-file-stream-64
  (sbfs-round-trip '(signed-byte 64) '(123456789012 -1 -9223372036854775808))
  (24 (123456789012 -1 -9223372036854775808) (123456789012 -1 -9223372036854775808)))

(deftest unsigned-byte-file-stream-64-top-bit
  (sbfs-round-trip '(unsigned-byte 64) '(1 18446744073709551615 9223372036854775808))
  (24 (1 18446744073709551615 9223372036854775808) (1 18446744073709551615 9223372036854775808)))

(deftest signed-byte-file-stream-128
  (sbfs-round-trip '(signed-byte 128) '(5 -1))
  (32 (5 -1) (5 -1)))

(deftest signed-byte-file-stream-write-sequence
  (let ((path (regression-temp-file "signed-byte-file-stream.bin")))
    (with-open-file (s path :direction :output :element-type '(signed-byte 16)
                            :if-exists :supersede)
      (write-sequence '(7 -2) s)
      (write-sequence (vector -4 5) s))
    (with-open-file (s path :element-type '(signed-byte 16))
      (let ((l (list 0 0 0 0)))
        (list (read-sequence l s) l))))
  (4 (7 -2 -4 5)))
