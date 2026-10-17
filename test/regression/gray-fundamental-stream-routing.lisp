;;; A Gray stream whose class names no direction (only FUNDAMENTAL-BINARY-STREAM
;;; or FUNDAMENTAL-CHARACTER-STREAM, with INPUT-STREAM-P / OUTPUT-STREAM-P
;;; methods) still sends the CL stream functions to the Gray generics, as SBCL
;;; does for every FUNDAMENTAL-STREAM.
(require "dotcl-gray")

(defclass %gfr-bin (dotcl-gray:fundamental-binary-stream)
  ((data :initarg :data :accessor %gfr-data)
   (out :initform nil :accessor %gfr-out)))
(defmethod input-stream-p ((s %gfr-bin)) t)
(defmethod output-stream-p ((s %gfr-bin)) t)
(defmethod dotcl-gray:stream-read-byte ((s %gfr-bin))
  (if (%gfr-data s) (pop (%gfr-data s)) :eof))
(defmethod dotcl-gray:stream-write-byte ((s %gfr-bin) b) (push b (%gfr-out s)) b)

(defclass %gfr-chr (dotcl-gray:fundamental-character-stream)
  ((data :initarg :data :accessor %gfr-cdata)
   (out :initform nil :accessor %gfr-cout)))
(defmethod input-stream-p ((s %gfr-chr)) t)
(defmethod dotcl-gray:stream-read-char ((s %gfr-chr))
  (if (%gfr-cdata s) (pop (%gfr-cdata s)) :eof))
(defmethod dotcl-gray:stream-unread-char ((s %gfr-chr) c) (push c (%gfr-cdata s)) nil)
(defmethod dotcl-gray:stream-write-char ((s %gfr-chr) c) (push c (%gfr-cout s)) c)

(deftest gray-fundamental-stream-routing.binary
  (let ((s (make-instance '%gfr-bin :data (list 1 2))))
    (list (streamp s) (open-stream-p s) (read-byte s) (read-byte s nil :end)
          (read-byte s nil :end) (write-byte 7 s) (%gfr-out s)
          (input-stream-p (make-two-way-stream s s))))
  (t t 1 2 :end 7 (7) t))

(deftest gray-fundamental-stream-routing.character
  (let ((s (make-instance '%gfr-chr :data (list #\a #\b))))
    (list (stream-element-type s) (read-char s) (unread-char #\a s)
          (read-char s) (read-char s) (read-char s nil :end) (write-char #\z s)
          (%gfr-cout s)))
  (character #\a nil #\a #\b :end #\z (#\z)))
