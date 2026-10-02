;;; WRITE-SEQUENCE has a two-argument direct entry (no argument array per call)
;;; alongside the variadic one that parses :start/:end. Both reach the same core,
;;; and the core resolves composite streams by recursing on the resolved stream
;;; instead of rewriting its argument array -- these pin that the two entries and
;;; every composite path still agree.

(deftest write-sequence.two-arg-string
  (with-output-to-string (s) (write-sequence "hi there!" s))
  "hi there!")

(deftest write-sequence.start-end
  (list (with-output-to-string (s) (write-sequence "hi there!" s :start 3))
        (with-output-to-string (s) (write-sequence "hi there!" s :end 2))
        (with-output-to-string (s) (write-sequence "hi there!" s :start 3 :end 8)))
  ("there!" "hi" "there"))

(deftest write-sequence.return-value-is-the-sequence
  (let* ((seq "abc") (r nil))
    (with-output-to-string (s) (setf r (write-sequence seq s)))
    (eq r seq))
  t)

(deftest write-sequence.list-and-vector
  (list (with-output-to-string (s) (write-sequence (list #\a #\b #\c) s))
        (with-output-to-string (s) (write-sequence (vector #\x #\y) s))
        (with-output-to-string (s) (write-sequence (list #\a #\b #\c #\d) s :start 1 :end 3)))
  ("abc" "xy" "bc"))

(deftest write-sequence.through-broadcast
  (let* ((a (make-string-output-stream))
         (b (make-string-output-stream))
         (bc (make-broadcast-stream a b)))
    (write-sequence "one" bc)
    (write-sequence "two!" bc :start 1 :end 3)
    (list (get-output-stream-string a) (get-output-stream-string b)))
  ("onewo" "onewo"))

(deftest write-sequence.through-two-way
  (let* ((out (make-string-output-stream))
         (tw (make-two-way-stream (make-string-input-stream "") out)))
    (write-sequence "abc" tw)
    (write-sequence "defg" tw :start 1)
    (get-output-stream-string out))
  "abcefg")

(deftest write-sequence.through-synonym
  (let ((out (make-string-output-stream)))
    (progv '(*ws-syn-target*) (list out)
      (let ((syn (make-synonym-stream '*ws-syn-target*)))
        (write-sequence "abc" syn)
        (write-sequence "xyz" syn :end 1)))
    (get-output-stream-string out))
  "abcx")

(deftest write-sequence.empty-and-nil
  (list (with-output-to-string (s) (write-sequence "" s))
        (with-output-to-string (s) (write-sequence nil s))
        (with-output-to-string (s) (write-sequence "ab" s :start 2)))
  ("" "" ""))

(deftest write-sequence.rejects-non-sequence
  (handler-case (with-output-to-string (s) (write-sequence 42 s))
    (type-error () :type-error)
    (error () :other))
  :type-error)

;;; An octet vector going to a binary file stream is written with one bulk
;;; Stream.Write. The per-element path flushed the file after every byte, so
;;; writing a 33 MB core image this way cost one write syscall per byte.
;;; These pin that the bulk path keeps :start/:end, fill pointers, the order
;;; against WRITE-BYTE, and that the bytes are visible to another handle as
;;; soon as WRITE-SEQUENCE returns (as they were with the per-byte flush).

(defun wsp-read-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (loop for b = (read-byte s nil) while b collect b)))

(deftest write-sequence.octets-to-file
  (let ((path (regression-temp-file "write-sequence-octets.bin"))
        (v (make-array 6 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 250 251 255))))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence v s)
      (write-byte 9 s)
      (write-sequence v s :start 2 :end 4)
      (write-sequence v s :start 5)
      (write-sequence v s :start 3 :end 3))
    (wsp-read-octets path))
  (1 2 3 250 251 255 9 3 250 255))

(deftest write-sequence.octets-fill-pointer
  (let ((path (regression-temp-file "write-sequence-octets-fp.bin"))
        (v (make-array 8 :element-type '(unsigned-byte 8) :fill-pointer 3
                         :initial-contents '(10 20 30 40 50 60 70 80))))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence v s))
    (wsp-read-octets path))
  (10 20 30))

(deftest write-sequence.octets-visible-before-close
  (let ((path (regression-temp-file "write-sequence-octets-open.bin"))
        (v (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(7 6 5 4))))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence v s)
      (list (wsp-read-octets path) (file-length s) (file-position s))))
  ((7 6 5 4) 4 4))

(deftest write-sequence.octets-displaced
  (let* ((path (regression-temp-file "write-sequence-octets-disp.bin"))
         (base (make-array 6 :element-type '(unsigned-byte 8)
                             :initial-contents '(1 2 3 4 5 6)))
         (v (make-array 3 :element-type '(unsigned-byte 8)
                          :displaced-to base :displaced-index-offset 2)))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence v s))
    (wsp-read-octets path))
  (3 4 5))

;;; WRITE-BYTE to a file stream no longer flushes the file after every byte
;;; (one write syscall per byte; the SBCL cross-build writes its fasls this way).
;;; The bytes stay in the stream's buffer until FINISH-OUTPUT, CLOSE, or
;;; FILE-LENGTH, and FILE-POSITION still counts them.

(deftest write-byte.file-buffered-then-flushed
  (let ((path (regression-temp-file "write-byte-buffered.bin")))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (dotimes (i 5) (write-byte (+ 100 i) s))
      (let ((pos (file-position s)))
        (finish-output s)
        (let ((seen (wsp-read-octets path)))
          (write-byte 1 s)
          (list pos seen (file-length s) (file-position s))))))
  (5 (100 101 102 103 104) 6 6))

(deftest write-byte.file-closed-content
  (let ((path (regression-temp-file "write-byte-closed.bin"))
        (v (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(7 8))))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (dotimes (i 3000) (write-byte (mod i 256) s))
      (write-sequence v s)
      (write-byte 9 s))
    (let ((got (wsp-read-octets path)))
      (list (length got) (nth 255 got) (nth 256 got) (last got 3))))
  (3003 255 0 (7 8 9)))
