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

;; Bivalent gray stream: a binary output stream that also defines stream-write-char.
;; cl:write-char must dispatch to stream-write-char regardless of element-type
;; (SBCL behavior). Previously write-char to a binary-only gray stream silently
;; leaked to the console because GetTextWriter only recognized character gray streams.
(defclass %gray-bivalent-out (dotcl-gray:fundamental-binary-output-stream)
  ((acc :initform nil :accessor %biv-out-acc)))
(defmethod dotcl-gray:stream-write-char ((s %gray-bivalent-out) ch)
  (push ch (%biv-out-acc s))
  ch)
(defmethod dotcl-gray:stream-write-string ((s %gray-bivalent-out) str &optional (start 0) end)
  (loop for i from start below (or end (length str))
        do (push (char str i) (%biv-out-acc s)))
  str)
(defmethod dotcl-gray:stream-write-byte ((s %gray-bivalent-out) byte)
  (push byte (%biv-out-acc s))
  byte)

(deftest i431-binary-gray-write-char
  (let ((s (make-instance '%gray-bivalent-out)))
    (list (write-char #\b s)                 ; returns the char
          (reverse (%biv-out-acc s))))       ; and dispatched to stream-write-char
  (#\b (#\b)))

;; The whole character-output family routes through the gray protocol for a
;; binary (bivalent) gray output stream, not just write-char.
(deftest i431-binary-gray-write-string
  (let ((s (make-instance '%gray-bivalent-out)))
    (write-string "hi" s)
    (reverse (%biv-out-acc s)))
  (#\h #\i))

(deftest i431-binary-gray-format
  (let ((s (make-instance '%gray-bivalent-out)))
    (format s "a~Ab" 7)
    (reverse (%biv-out-acc s)))
  (#\a #\7 #\b))

(deftest i431-binary-gray-princ
  (let ((s (make-instance '%gray-bivalent-out)))
    (princ "xy" s)
    (reverse (%biv-out-acc s)))
  (#\x #\y))

;; write-byte on the same bivalent stream still dispatches to stream-write-byte.
(deftest i431-binary-gray-write-byte-still-works
  (let ((s (make-instance '%gray-bivalent-out)))
    (write-byte 66 s)
    (reverse (%biv-out-acc s)))
  (66))

;;; ------------------------------------------------------------------
;;; read-sequence / write-sequence dispatch to the Gray stream generics
;;; (dotcl-gray:stream-read-sequence / stream-write-sequence). CL:READ-SEQUENCE
;;; must honor a specialized method; an unspecialized stream falls back to the
;;; per-element char/byte loop (matching the prior behavior).
;;; ------------------------------------------------------------------

(defclass %rs-str-in (dotcl-gray:fundamental-character-input-stream)
  ((data :initarg :data) (pos :initform 0) (bulk :initform nil :accessor %rs-bulk)))
(defmethod dotcl-gray:stream-read-char ((s %rs-str-in))
  (with-slots (data pos) s
    (if (< pos (length data)) (prog1 (char data pos) (incf pos)) :eof)))
(defmethod dotcl-gray:stream-read-sequence ((s %rs-str-in) seq &optional (start 0) (end (length seq)))
  (setf (%rs-bulk s) t)
  (with-slots (data pos) s
    (do ((i start (1+ i))) ((>= i end) i)
      (if (< pos (length data)) (progn (setf (elt seq i) (char data pos)) (incf pos)) (return i)))))

;; A specialized stream-read-sequence is invoked by CL:READ-SEQUENCE.
(deftest gray-read-sequence-specialized-dispatch
  (let* ((s (make-instance '%rs-str-in :data "hello")) (buf (make-string 5)))
    (let ((n (read-sequence buf s))) (list n buf (%rs-bulk s))))
  (5 "hello" t))

(defclass %rs-str-in2 (dotcl-gray:fundamental-character-input-stream)
  ((data :initarg :data) (pos :initform 0)))
(defmethod dotcl-gray:stream-read-char ((s %rs-str-in2))
  (with-slots (data pos) s
    (if (< pos (length data)) (prog1 (char data pos) (incf pos)) :eof)))

;; No specialization: default method reads char-by-char and stops at EOF.
(deftest gray-read-sequence-default-eof
  (let* ((s (make-instance '%rs-str-in2 :data "hi")) (buf (make-string 5)))
    (let ((n (read-sequence buf s))) (list n (subseq buf 0 n))))
  (2 "hi"))

(defclass %rs-str-out (dotcl-gray:fundamental-character-output-stream)
  ((acc :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
        :accessor %rs-out-acc)))
(defmethod dotcl-gray:stream-write-char ((s %rs-str-out) c) (vector-push-extend c (%rs-out-acc s)) c)

;; write-sequence to a Gray character output stream reaches stream-write-char.
(deftest gray-write-sequence-char
  (let ((s (make-instance '%rs-str-out)))
    (write-sequence "world" s)
    (coerce (%rs-out-acc s) 'string))
  "world")

(defclass %rs-byte-in (dotcl-gray:fundamental-binary-input-stream)
  ((data :initarg :data) (pos :initform 0)))
(defmethod dotcl-gray:stream-read-byte ((s %rs-byte-in))
  (with-slots (data pos) s
    (if (< pos (length data)) (prog1 (aref data pos) (incf pos)) :eof)))

;; Binary Gray input stream: default read-sequence loops stream-read-byte.
(deftest gray-read-sequence-binary-default
  (let* ((s (make-instance '%rs-byte-in :data #(10 20 30))) (buf (make-array 5)))
    (let ((n (read-sequence buf s))) (list n (coerce (subseq buf 0 n) 'list))))
  (3 (10 20 30)))

;;; ------------------------------------------------------------------
;;; cl:read-char / cl:peek-char dispatch to the Gray character input generics.
;;; Previously ResolveLispStream dropped a gray CLOS instance to *standard-input*
;;; (default: branch), so read-char returned :EOF and peek-char stack-overflowed.
;;; ------------------------------------------------------------------

(defclass %rc-str-in (dotcl-gray:fundamental-character-input-stream)
  ((data :initarg :data) (pos :initform 0)))
(defmethod dotcl-gray:stream-read-char ((s %rc-str-in))
  (with-slots (data pos) s
    (if (< pos (length data)) (prog1 (char data pos) (incf pos)) :eof)))
(defmethod dotcl-gray:stream-unread-char ((s %rc-str-in) ch)
  (declare (ignore ch))
  (with-slots (pos) s (when (> pos 0) (decf pos)) nil))

(deftest gray-read-char-dispatch
  (let ((s (make-instance '%rc-str-in :data "abc")))
    (list (read-char s) (read-char s) (read-char s) (read-char s nil :eof)))
  (#\a #\b #\c :eof))

(deftest gray-peek-char-nil-no-consume
  (let ((s (make-instance '%rc-str-in :data "xy")))
    (list (peek-char nil s) (read-char s) (read-char s)))
  (#\x #\x #\y))

(deftest gray-peek-char-target
  (let ((s (make-instance '%rc-str-in :data "aabca")))
    (list (peek-char #\c s) (read-char s)))
  (#\c #\c))

(deftest gray-read-line-over-read-char
  (let ((s (make-instance '%rc-str-in :data (format nil "hi~%yo"))))
    (multiple-value-list (read-line s nil nil)))
  ("hi" nil))

;;; open-stream-p must dispatch to a user-defined method on a gray stream, not
;;; short-circuit to the builtin gray default (which always returns T). This is
;;; what flexi-streams relies on (its open-stream-p delegates to the underlying
;;; stream); the direct Runtime.OpenStreamP compile-inline previously bypassed it.
(defclass %osp-gray (dotcl-gray:fundamental-character-input-stream)
  ((open :initarg :open :accessor %osp-open)))
(defmethod open-stream-p ((s %osp-gray)) (%osp-open s))
(deftest gray-open-stream-p-dispatches-user-method
  (list (notnot (open-stream-p (make-instance '%osp-gray :open t)))
        (open-stream-p (make-instance '%osp-gray :open nil)))
  (t nil))

;;; ------------------------------------------------------------------
;;; file-position must dispatch to stream-file-position /
;;; (setf stream-file-position) generics for Gray streams.
;;; Previously both getter and setter fell through to return NIL.
;;; ------------------------------------------------------------------

;; A Gray character input stream tracking a position slot.
(defclass %fp-gray-in (dotcl-gray:fundamental-character-input-stream)
  ((data :initarg :data) (pos :initform 0)))
(defmethod dotcl-gray:stream-read-char ((s %fp-gray-in))
  (with-slots (data pos) s
    (if (< pos (length data)) (prog1 (char data pos) (incf pos)) :eof)))
(defmethod dotcl-gray:stream-file-position ((s %fp-gray-in))
  (slot-value s 'pos))
(defmethod (setf dotcl-gray:stream-file-position) (newval (s %fp-gray-in))
  (setf (slot-value s 'pos) newval))

(deftest gray-file-position-getter
  (let ((s (make-instance '%fp-gray-in :data "hello")))
    (read-char s)
    (file-position s))
  1)

(deftest gray-file-position-setf
  (let ((s (make-instance '%fp-gray-in :data "hello")))
    (read-char s)
    (read-char s)
    (file-position s 0)
    (read-char s))
  #\h)

(deftest gray-file-position-setf-returns-position
  (let ((s (make-instance '%fp-gray-in :data "hello")))
    (file-position s 3))
  3)

;;; make-two-way-stream / make-echo-stream must accept Gray streams. Previously
;;; they type-checked the C# LispStream class and rejected a Gray input/output
;;; stream even though streamp / input-stream-p / output-stream-p answer T for it
;;; (self-contradiction). Reads/writes through the composite reach the Gray
;;; components, and the component accessors return the original object (identity).
(defclass %gray-src (dotcl-gray:fundamental-character-input-stream)
  ((chars :initarg :chars :accessor %gray-src-chars)))
(defmethod dotcl-gray:stream-read-char ((s %gray-src))
  (if (%gray-src-chars s) (pop (%gray-src-chars s)) :eof))

(deftest gray-two-way-accepts
  (let ((in (make-instance '%gray-src :chars '(#\a)))
        (out (make-instance '%gray-acc)))
    (streamp (make-two-way-stream in out)))
  t)

(deftest gray-two-way-read-write-through
  (let* ((in (make-instance '%gray-src :chars (coerce "hi" 'list)))
         (out (make-instance '%gray-acc))
         (tw (make-two-way-stream in out)))
    (write-char #\X tw)
    (list (read-char tw) (read-char tw)
          (get-output-stream-string (%gray-acc-buf out))))
  (#\h #\i "X"))

(deftest gray-two-way-input-identity
  (let* ((in (make-instance '%gray-src :chars nil))
         (out (make-instance '%gray-acc))
         (tw (make-two-way-stream in out)))
    (eq (two-way-stream-input-stream tw) in))
  t)

(deftest gray-two-way-output-identity
  (let* ((in (make-instance '%gray-src :chars nil))
         (out (make-instance '%gray-acc))
         (tw (make-two-way-stream in out)))
    (eq (two-way-stream-output-stream tw) out))
  t)

(deftest gray-echo-accepts
  (let ((in (make-instance '%gray-src :chars '(#\z)))
        (out (make-instance '%gray-acc)))
    (streamp (make-echo-stream in out)))
  t)

(deftest gray-concatenated-accepts
  (let ((in (make-instance '%gray-src :chars '(#\q))))
    (streamp (make-concatenated-stream in)))
  t)

;;; clear-input / stream-element-type / stream-external-format must accept Gray
;;; streams (streamp / input-stream-p already answer T for them). Previously they
;;; used a bare "is not LispStream" check and rejected a Gray input as "not a
;;; stream", which broke the swank/micros REPL (listener-eval calls clear-input).
(deftest gray-clear-input-accepts
  (let ((in (make-instance '%gray-in)))
    (clear-input in))
  nil)

(deftest gray-stream-element-type-character
  (let ((in (make-instance '%gray-in)))
    (stream-element-type in))
  character)

(deftest gray-stream-element-type-output
  (let ((out (make-instance '%gray-out)))
    (stream-element-type out))
  character)

(deftest gray-stream-external-format-accepts
  (let ((in (make-instance '%gray-in)))
    (stream-external-format in))
  :default)

;; Non-stream objects still error on these (the check moved, not removed).
(deftest gray-clear-input-non-stream-errors
  (handler-case (progn (clear-input 42) :no-error)
    (error () :errored))
  :errored)

(deftest gray-stream-element-type-non-stream-errors
  (handler-case (progn (stream-element-type 42) :no-error)
    (error () :errored))
  :errored)
