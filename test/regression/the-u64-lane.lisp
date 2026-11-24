;;; (THE (UNSIGNED-BYTE 64) X) around an expression the 64-bit lane has proved to
;;; lie in [0, 2^64) is the expression itself: no check, no box. The values must
;;; be the generic ones; a form whose value can leave the type keeps its check at
;;; SAFETY 3, and a narrower THE type does not take the shortcut (the bound of the word would not fit it).

(deftype tu64-lane () '(unsigned-byte 64))

(defun tu64-pack (input index)
  "Eight octets of INPUT from INDEX as one word, little endian, each term under THE."
  (declare (type (simple-array (unsigned-byte 8) (*)) input) (type fixnum index)
           (optimize (speed 3) (space 0) (safety 0)))
  (logxor (the tu64-lane (ash (aref input index) 0))
          (the tu64-lane (ash (aref input (+ index 1)) 8))
          (the tu64-lane (ash (aref input (+ index 2)) 16))
          (the tu64-lane (ash (aref input (+ index 3)) 24))
          (the tu64-lane (ash (aref input (+ index 4)) 32))
          (the tu64-lane (ash (aref input (+ index 5)) 40))
          (the tu64-lane (ash (aref input (+ index 6)) 48))
          (the (unsigned-byte 64) (ash (aref input (+ index 7)) 56))))

(defun tu64-top (b)
  (declare (type (unsigned-byte 8) b) (optimize (speed 3) (safety 0)))
  (logxor 0 (the (unsigned-byte 64) (ash b 56))))

(deftest the-u64-lane.words-from-octets
  (let ((v (make-array 16 :element-type '(unsigned-byte 8)
                          :initial-contents '(1 2 3 4 5 6 7 8 255 254 253 252 251 250 249 248))))
    (list (tu64-pack v 0) (tu64-pack v 8)))
  (#x0807060504030201 #xF8F9FAFBFCFDFEFF))

(deftest the-u64-lane.high-bit-set
  (list (tu64-top 255) (tu64-top 128) (tu64-top 0) (tu64-top 1))
  (#xFF00000000000000 #x8000000000000000 0 #x0100000000000000))

;;; A value outside the type: the check of SAFETY 3 is kept, and the message is the usual one.
(defun tu64-out (b)
  (declare (type (unsigned-byte 8) b) (optimize (safety 3)))
  (logxor 0 (the (unsigned-byte 64) (ash b 64))))

(defun tu64-narrow-ok (b)
  (declare (type (unsigned-byte 8) b) (optimize (safety 3)))
  (logxor 0 (the (unsigned-byte 8) (ash b 0))))

(deftest-emitting-only the-u64-lane.out-of-type-still-errors-at-safety-3
  (list (handler-case (tu64-out 1) (type-error () :type-error))
        (handler-case (tu64-narrow-ok 200) (type-error () :type-error))
        (handler-case (tu64-out 0) (type-error () :type-error)))
  (:type-error 200 0))

;;; A signed difference is not proved to lie in the type.
(defun tu64-diff (a b)
  (declare (type (unsigned-byte 64) a b) (optimize (safety 3)))
  (logxor 0 (the (unsigned-byte 64) (- a b))))

(deftest-emitting-only the-u64-lane.signed-difference-is-checked
  (list (tu64-diff 5 3) (handler-case (tu64-diff 3 5) (type-error () :type-error)))
  (2 :type-error))

;;; A narrower unsigned type: the shortcut only where the bound of the word fits it.
(defun tu64-w32 (b)
  (declare (type (unsigned-byte 8) b) (optimize (speed 3) (safety 0)))
  (logxor 0 (the (unsigned-byte 32) (ash b 24))))

(defun tu64-w32-wide (b)
  (declare (type (unsigned-byte 8) b) (optimize (speed 3) (safety 0)))
  (logxor 0 (the (unsigned-byte 32) (ash b 25))))

(deftest the-u64-lane.narrow-types
  (list (tu64-w32 255) (tu64-w32 1) (tu64-w32-wide 1))
  (#xFF000000 #x1000000 #x2000000))
