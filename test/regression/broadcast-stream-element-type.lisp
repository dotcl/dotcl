;;; STREAM-ELEMENT-TYPE of a broadcast stream is the element type of its last
;;; component stream, and T when it has no component (CLHS, Broadcast Streams).

(deftest broadcast-stream-element-type.empty
  (stream-element-type (make-broadcast-stream))
  t)

(deftest broadcast-stream-element-type.last-component
  (list (stream-element-type (make-broadcast-stream (make-string-output-stream)))
        (stream-element-type (make-broadcast-stream (make-string-output-stream)
                                                    (make-string-output-stream)))
        (let ((path (regression-temp-file "broadcast-element-type.bin")))
          (with-open-file (b path :direction :output :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
            (stream-element-type (make-broadcast-stream (make-string-output-stream) b)))))
  (character character (unsigned-byte 8)))

(deftest broadcast-stream-element-type.nested
  (stream-element-type (make-broadcast-stream (make-broadcast-stream)))
  t)
