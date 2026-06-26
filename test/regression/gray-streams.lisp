;;; Regression tests for Gray Streams predicate consistency

;;; open-stream-p must accept Gray Streams instances.
;;; Previously (streamp gs) => T but (open-stream-p gs) signaled
;;; "OPEN-STREAM-P: not a stream". open-stream-p now mirrors streamp:
;;; a gray input/output stream is reported open.
(require "dotcl-gray")

(defclass %gray-out (dotcl-gray:fundamental-character-output-stream) ())
(defclass %gray-in  (dotcl-gray:fundamental-character-input-stream) ())

(deftest d1211-gray-out-streamp
  (streamp (make-instance '%gray-out))
  t)

(deftest d1211-gray-out-open-stream-p
  (open-stream-p (make-instance '%gray-out))
  t)

(deftest d1211-gray-out-output-stream-p
  (output-stream-p (make-instance '%gray-out))
  t)

(deftest d1211-gray-in-streamp
  (streamp (make-instance '%gray-in))
  t)

(deftest d1211-gray-in-open-stream-p
  (open-stream-p (make-instance '%gray-in))
  t)

(deftest d1211-gray-in-input-stream-p
  (input-stream-p (make-instance '%gray-in))
  t)

;;; Non-stream objects still signal a type error.
(deftest d1211-open-stream-p-non-stream
  (handler-case (progn (open-stream-p 42) :no-error)
    (error () :errored))
  :errored)

;;; force-output / finish-output / clear-output trampoline to the gray
;;; generics (stream-force-output etc.) instead of "not a stream designator".
(deftest d1217-gray-force-output
  (handler-case (progn (force-output (make-instance '%gray-out)) :ok)
    (error () :errored))
  :ok)

(deftest d1217-gray-finish-output
  (handler-case (progn (finish-output (make-instance '%gray-out)) :ok)
    (error () :errored))
  :ok)

(deftest d1217-gray-clear-output
  (handler-case (progn (clear-output (make-instance '%gray-out)) :ok)
    (error () :errored))
  :ok)

;;; (format gray-stream ...) must write through the Gray protocol
;;; (stream-write-string / stream-write-char) instead of signaling
;;; "FORMAT: invalid destination". A capturing Gray output stream:
(defclass %gray-acc (dotcl-gray:fundamental-character-output-stream)
  ((buf :initform (make-string-output-stream) :reader %gray-acc-buf)))
(defmethod dotcl-gray:stream-write-char ((s %gray-acc) ch)
  (write-char ch (%gray-acc-buf s)))
(defmethod dotcl-gray:stream-write-string ((s %gray-acc) str &optional (start 0) end)
  (write-string str (%gray-acc-buf s) :start start :end end) str)

;; format with a simple directive lands in the gray buffer.
(deftest d312-format-gray-basic
  (let ((s (make-instance '%gray-acc)))
    (format s "via-format ~d" 42)
    (get-output-stream-string (%gray-acc-buf s)))
  "via-format 42")

;; format and write-string interleave correctly on the same gray stream.
(deftest d312-format-gray-interleave
  (let ((s (make-instance '%gray-acc)))
    (write-string "A" s)
    (format s "~a" "B")
    (write-string "C" s)
    (get-output-stream-string (%gray-acc-buf s)))
  "ABC")

;; format returns NIL when writing to a stream (not capturing a string).
(deftest d312-format-gray-returns-nil
  (let ((s (make-instance '%gray-acc)))
    (format s "x"))
  nil)

;; ~% (newline) goes through stream-write-char on the gray stream.
(deftest d312-format-gray-newline
  (let ((s (make-instance '%gray-acc)))
    (format s "a~%b")
    (get-output-stream-string (%gray-acc-buf s)))
  "a
b")

;; (format t ...) when *standard-output* is bound to a gray stream must
;; reach the gray stream (GetTextWriter trampoline), not leak to the console.
;; The explicit-stream form already worked; only the t/*standard-output*
;; path fell through to Console.Write.
(deftest i318-format-t-gray-standard-output
  (let ((s (make-instance '%gray-acc)))
    (let ((*standard-output* s))
      (write-string "WS" s)
      (format s "FMT-STREAM ")
      (format t "FMT-T "))
    (get-output-stream-string (%gray-acc-buf s)))
  "WSFMT-STREAM FMT-T ")

;;; interactive-stream-p must accept Gray Streams instances (mirrors d1211).
;;; Previously it signaled "INTERACTIVE-STREAM-P: not a stream" on a gray
;;; (CLOS) stream instead of returning NIL.  This broke ACCEPT-FROM-STRING
;;; (its string-input-stream is a gray stream) and thus McCLIM ACCEPTING-VALUES.
(deftest i319-interactive-stream-p-gray-out
  (interactive-stream-p (make-instance '%gray-out))
  nil)

(deftest i319-interactive-stream-p-gray-in
  (interactive-stream-p (make-instance '%gray-in))
  nil)

;;; A real string-input-stream (also a gray stream in dotcl) is not interactive.
(deftest i319-interactive-stream-p-string-input
  (with-input-from-string (s "abc")
    (interactive-stream-p s))
  nil)

;;; Non-stream objects still signal a type error.
(deftest i319-interactive-stream-p-non-stream
  (handler-case (progn (interactive-stream-p 42) :no-error)
    (error () :errored))
  :errored)

;;; the printer (princ/prin1/print/write) must also funnel through the
;;; Gray protocol. FORMAT's Format.cs path was fixed, but princ/prin1/print/write
;;; go through a separate GetOutputWriter resolver that had no gray branch and
;;; leaked to Console.Out, bypassing stream-write-char/string. (Broke McCLIM
;;; menu-choose, whose print-menu-item does (princ display stream).)
(deftest i320-princ-string-gray
  (let ((s (make-instance '%gray-acc)))
    (princ "EEE" s)
    (get-output-stream-string (%gray-acc-buf s)))
  "EEE")

(deftest i320-princ-symbol-gray
  (let ((s (make-instance '%gray-acc)))
    (princ 'fff s)
    (get-output-stream-string (%gray-acc-buf s)))
  "FFF")

(deftest i320-prin1-string-gray
  (let ((s (make-instance '%gray-acc)))
    (prin1 "GGG" s)
    (get-output-stream-string (%gray-acc-buf s)))
  "\"GGG\"")

(deftest i320-write-number-gray
  (let ((s (make-instance '%gray-acc)))
    (write 123 :stream s)
    (get-output-stream-string (%gray-acc-buf s)))
  "123")

;; print writes a leading newline + the object + a trailing space; compare as a
;; char list so the expected form is a plain literal (deftest does not eval it).
(deftest i320-print-gray
  (let ((s (make-instance '%gray-acc)))
    (print 42 s)
    (coerce (get-output-stream-string (%gray-acc-buf s)) 'list))
  (#\Newline #\4 #\2 #\Space))

;;; gray detection must survive a SAME-NAMED class in another package
;;; (e.g. trivial-gray-streams defines its own FUNDAMENTAL-CHARACTER-OUTPUT-STREAM
;;; after asdf load). The old code looked the name up via FindClassByName (first
;;; registry match) and reference-compared it against the instance's CPL, which
;;; broke order-dependently once a duplicate name existed: (streamp s) => T but
;;; (open-stream-p s) signaled "not a stream". Detection is now name-based over
;;; the precedence list. The two subclasses below cover both duplicate classes;
;;; the old reference-compare could only ever satisfy ONE of them, so at least one
;;; of these tests deterministically failed before the fix.
(defpackage :tgs-321 (:use))
(defclass tgs-321::fundamental-character-output-stream () ())
(defclass tgs-321::fundamental-character-input-stream () ())
(defclass %gray-out-321 (dotcl-gray:fundamental-character-output-stream) ())
(defclass %gray-out-321-dup (tgs-321::fundamental-character-output-stream) ())
(defclass %gray-in-321 (dotcl-gray:fundamental-character-input-stream) ())
(defclass %gray-in-321-dup (tgs-321::fundamental-character-input-stream) ())

(deftest i321-gray-out-builtin-detected-with-dup
  (open-stream-p (make-instance '%gray-out-321))
  t)

(deftest i321-gray-out-dupnamed-detected
  (open-stream-p (make-instance '%gray-out-321-dup))
  t)

(deftest i321-gray-in-builtin-detected-with-dup
  (open-stream-p (make-instance '%gray-in-321))
  t)

(deftest i321-gray-in-dupnamed-detected
  (open-stream-p (make-instance '%gray-in-321-dup))
  t)

;;; read-byte/write-byte must dispatch to the gray binary stream
;;; generics (stream-read-byte / stream-write-byte). Previously read-byte/
;;; write-byte on a fundamental-binary-{input,output}-stream signaled
;;; "not a stream" because dotcl's gray dispatch covered character only.
;;; This is the gap that blocked the real flexi-streams in-memory streams.

;; A gray binary input stream backed by a fixed octet vector.
(defclass %gray-bin-in (dotcl-gray:fundamental-binary-input-stream)
  ((bytes :initarg :bytes :accessor %bin-in-bytes)
   (pos :initform 0 :accessor %bin-in-pos)))
(defmethod dotcl-gray:stream-read-byte ((s %gray-bin-in))
  (let ((p (%bin-in-pos s)) (v (%bin-in-bytes s)))
    (if (< p (length v))
        (progn (setf (%bin-in-pos s) (1+ p)) (aref v p))
        :eof)))

;; A gray binary output stream that accumulates bytes into a list.
(defclass %gray-bin-out (dotcl-gray:fundamental-binary-output-stream)
  ((acc :initform nil :accessor %bin-out-acc)))
(defmethod dotcl-gray:stream-write-byte ((s %gray-bin-out) byte)
  (push byte (%bin-out-acc s))
  byte)

(deftest i360-gray-bin-streamp
  (streamp (make-instance '%gray-bin-in :bytes #(1 2)))
  t)

(deftest i360-gray-bin-in-input-stream-p
  (input-stream-p (make-instance '%gray-bin-in :bytes #(1 2)))
  t)

(deftest i360-gray-bin-out-output-stream-p
  (output-stream-p (make-instance '%gray-bin-out))
  t)

(deftest i360-gray-read-byte
  (let ((s (make-instance '%gray-bin-in :bytes #(104 105))))
    (list (read-byte s) (read-byte s)))
  (104 105))

;; read-byte at EOF with eof-error-p nil returns the eof-value.
(deftest i360-gray-read-byte-eof
  (let ((s (make-instance '%gray-bin-in :bytes #(7))))
    (read-byte s)
    (read-byte s nil :the-end))
  :the-end)

;; read-byte at EOF with eof-error-p t signals end-of-file.
(deftest i360-gray-read-byte-eof-error
  (let ((s (make-instance '%gray-bin-in :bytes #())))
    (handler-case (progn (read-byte s) :no-error)
      (end-of-file () :eof-error)))
  :eof-error)

(deftest i360-gray-write-byte
  (let ((s (make-instance '%gray-bin-out)))
    (write-byte 65 s)
    (write-byte 66 s)
    (reverse (%bin-out-acc s)))
  (65 66))

;; write-byte returns the byte written.
(deftest i360-gray-write-byte-return
  (write-byte 99 (make-instance '%gray-bin-out))
  99)

;; read-sequence into a vector dispatches per-byte to stream-read-byte.
(deftest i360-gray-read-sequence
  (let ((s (make-instance '%gray-bin-in :bytes #(10 20 30)))
        (buf (make-array 3)))
    (let ((n (read-sequence buf s)))
      (list n (coerce buf 'list))))
  (3 (10 20 30)))

;; read-sequence stops at EOF and returns the fill index.
(deftest i360-gray-read-sequence-short
  (let ((s (make-instance '%gray-bin-in :bytes #(1 2)))
        (buf (make-array 4 :initial-element 0)))
    (let ((n (read-sequence buf s)))
      (list n (coerce buf 'list))))
  (2 (1 2 0 0)))

;; write-sequence dispatches per-byte to stream-write-byte.
(deftest i360-gray-write-sequence
  (let ((s (make-instance '%gray-bin-out)))
    (write-sequence #(7 8 9) s)
    (reverse (%bin-out-acc s)))
  (7 8 9))
