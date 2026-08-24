;;; dotcl-gray.lisp — Gray stream implementation for dotcl
;;;
;;; Usage: (require "dotcl-gray")
;;;
;;; Provides DOTCL-GRAY package with the full Gray stream class hierarchy
;;; and generic functions (AMOP/Gray streams protocol).
;;; The C# runtime (GrayStream.cs, Runtime.Misc.cs) dispatches to these
;;; generic functions when a CLOS instance inheriting from the classes
;;; defined here is passed to CL stream functions (read-char, write-char, etc.).

(defpackage :dotcl-gray
  (:use :cl)
  (:export
   ;; Classes
   #:fundamental-stream
   #:fundamental-input-stream
   #:fundamental-output-stream
   #:fundamental-character-stream
   #:fundamental-binary-stream
   #:fundamental-character-input-stream
   #:fundamental-character-output-stream
   #:fundamental-binary-input-stream
   #:fundamental-binary-output-stream
   ;; Character input GFs
   #:stream-read-char
   #:stream-unread-char
   #:stream-read-char-no-hang
   #:stream-peek-char
   #:stream-listen
   #:stream-read-line
   #:stream-clear-input
   ;; Character output GFs
   #:stream-write-char
   #:stream-line-column
   #:stream-start-line-p
   #:stream-write-string
   #:stream-terpri
   #:stream-fresh-line
   #:stream-finish-output
   #:stream-force-output
   #:stream-clear-output
   #:stream-advance-to-column
   ;; Binary GFs
   #:stream-read-byte
   #:stream-write-byte
   #:stream-read-sequence
   #:stream-write-sequence
   ;; File position GF
   #:stream-file-position))

(in-package :dotcl-gray)

;;; ============================================================
;;; Class hierarchy (Gray streams / AMOP Chapter 15)
;;; ============================================================

(defclass fundamental-stream (stream) ()
  (:documentation "Base class for all Gray streams."))

(defclass fundamental-input-stream (fundamental-stream) ()
  (:documentation "Base class for Gray input streams."))

(defclass fundamental-output-stream (fundamental-stream) ()
  (:documentation "Base class for Gray output streams."))

(defclass fundamental-character-stream (fundamental-stream) ()
  (:documentation "Base class for Gray character streams."))

(defclass fundamental-binary-stream (fundamental-stream) ()
  (:documentation "Base class for Gray binary streams."))

(defclass fundamental-character-input-stream
    (fundamental-input-stream fundamental-character-stream) ()
  (:documentation "Base class for Gray character input streams."))

(defclass fundamental-character-output-stream
    (fundamental-output-stream fundamental-character-stream) ()
  (:documentation "Base class for Gray character output streams."))

(defclass fundamental-binary-input-stream
    (fundamental-input-stream fundamental-binary-stream) ()
  (:documentation "Base class for Gray binary input streams."))

(defclass fundamental-binary-output-stream
    (fundamental-output-stream fundamental-binary-stream) ()
  (:documentation "Base class for Gray binary output streams."))

;;; ============================================================
;;; Character input generic functions
;;; ============================================================

(defgeneric stream-read-char (stream)
  (:documentation "Read one character from STREAM. Return :EOF at end of file."))

(defgeneric stream-unread-char (stream character)
  (:documentation "Put CHARACTER back into STREAM (undo last stream-read-char)."))

(defgeneric stream-read-char-no-hang (stream)
  (:documentation "Read a character if available without blocking. Return NIL if none, :EOF at end."))

(defgeneric stream-peek-char (stream)
  (:documentation "Peek at the next character without consuming it. Return :EOF at end."))

(defgeneric stream-listen (stream)
  (:documentation "Return true if input is available on STREAM without blocking."))

(defgeneric stream-read-line (stream)
  (:documentation "Read a line from STREAM. Return (values string eof-p)."))

(defgeneric stream-clear-input (stream)
  (:documentation "Clear any buffered input on STREAM."))

;;; ============================================================
;;; Character output generic functions
;;; ============================================================

(defgeneric stream-write-char (stream character)
  (:documentation "Write CHARACTER to STREAM."))

(defgeneric stream-line-column (stream)
  (:documentation "Return the current column number of STREAM, or NIL if unknown."))

(defgeneric stream-start-line-p (stream)
  (:documentation "Return true if STREAM is at the start of a line."))

(defgeneric stream-write-string (stream string &optional start end)
  (:documentation "Write STRING (or the subseq from START to END) to STREAM."))

(defgeneric stream-terpri (stream)
  (:documentation "Output a newline to STREAM."))

(defgeneric stream-fresh-line (stream)
  (:documentation "Output a newline to STREAM unless already at start of line."))

(defgeneric stream-finish-output (stream)
  (:documentation "Force output and wait until it completes."))

(defgeneric stream-force-output (stream)
  (:documentation "Force any buffered output to be sent without waiting."))

(defgeneric stream-clear-output (stream)
  (:documentation "Abort any queued output on STREAM."))

(defgeneric stream-advance-to-column (stream column)
  (:documentation "Output whitespace to advance STREAM to COLUMN."))

;;; ============================================================
;;; Binary stream generic functions
;;; ============================================================

(defgeneric stream-read-byte (stream)
  (:documentation "Read one byte from STREAM. Return :EOF at end of file."))

(defgeneric stream-write-byte (stream byte)
  (:documentation "Write BYTE (an integer) to STREAM."))

;;; ============================================================
;;; Bulk sequence generic functions
;;; ============================================================
;;;
;;; CL:READ-SEQUENCE / WRITE-SEQUENCE dispatch here (via the C# runtime) when
;;; the stream is a Gray stream, so a subclass can provide an efficient bulk
;;; implementation (e.g. flexi-streams decoding a whole octet buffer at once).
;;; The default methods below fall back to the per-element char/byte generics,
;;; matching the behavior of a stream that only implements stream-read-char etc.

(defgeneric stream-read-sequence (stream seq &optional start end)
  (:documentation "Fill SEQ from STREAM between START and END. Return the index
of the first element not updated (i.e. START + number of elements read)."))

(defgeneric stream-write-sequence (stream seq &optional start end)
  (:documentation "Write SEQ to STREAM between START and END. Return SEQ."))

(defgeneric stream-file-position (stream)
  (:documentation "Return the current file position of STREAM, or NIL if unknown."))

;;; ============================================================
;;; Default methods
;;; ============================================================

;;; Every other output GF here answers for a class that defines only
;;; stream-write-char, but stream-line-column had no default -- so a stream
;;; whose author did not track columns got "no applicable method" from
;;; stream-start-line-p (which asks for the column), and from fresh-line and
;;; format's ~T through it. The Gray protocol says an unknown column is NIL,
;;; and callers already read NIL as "unknown"; only the missing method turned
;;; that into an error.
(defmethod stream-line-column ((stream fundamental-output-stream))
  nil)

(defmethod stream-start-line-p ((stream fundamental-output-stream))
  (eql (stream-line-column stream) 0))

(defmethod stream-terpri ((stream fundamental-output-stream))
  (stream-write-char stream #\newline))

(defmethod stream-fresh-line ((stream fundamental-output-stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)
    t))

(defmethod stream-write-string ((stream fundamental-output-stream) string
                                &optional (start 0) (end (length string)))
  (loop for i from start below end
        do (stream-write-char stream (char string i)))
  string)

(defmethod stream-finish-output ((stream fundamental-output-stream))
  (stream-force-output stream))

(defmethod stream-force-output ((stream fundamental-output-stream))
  nil)

(defmethod stream-clear-output ((stream fundamental-output-stream))
  nil)

(defmethod stream-advance-to-column ((stream fundamental-output-stream) column)
  (let ((col (stream-line-column stream)))
    (when col
      (loop while (< col column)
            do (stream-write-char stream #\space)
               (incf col)))))

(defmethod stream-listen ((stream fundamental-input-stream))
  nil)

(defmethod stream-read-char-no-hang ((stream fundamental-input-stream))
  (stream-read-char stream))

;;; Default stream-peek-char: read a character and put it back. Requires the
;;; stream to implement stream-unread-char (as the Gray protocol expects for
;;; peekable character input streams).
(defmethod stream-peek-char ((stream fundamental-character-input-stream))
  (let ((c (stream-read-char stream)))
    (unless (eq c :eof)
      (stream-unread-char stream c))
    c))

;;; Default stream-file-position: return NIL (position unknown).
(defmethod stream-file-position ((stream fundamental-stream))
  nil)

(defmethod stream-clear-input ((stream fundamental-input-stream))
  nil)

;;; Default bulk methods: per-element loop over the char/byte generics.
;;; A stream that specializes stream-read-sequence/stream-write-sequence (e.g.
;;; flexi-streams) overrides these to decode/encode in bulk.

(defmethod stream-read-sequence ((stream fundamental-character-input-stream) seq
                                 &optional (start 0) (end (length seq)))
  (do ((i start (1+ i)))
      ((>= i end) i)
    (let ((c (stream-read-char stream)))
      (if (eq c :eof)
          (return i)
          (setf (elt seq i) c)))))

(defmethod stream-read-sequence ((stream fundamental-binary-input-stream) seq
                                 &optional (start 0) (end (length seq)))
  (do ((i start (1+ i)))
      ((>= i end) i)
    (let ((b (stream-read-byte stream)))
      (if (eq b :eof)
          (return i)
          (setf (elt seq i) b)))))

(defmethod stream-write-sequence ((stream fundamental-character-output-stream) seq
                                  &optional (start 0) (end (length seq)))
  (do ((i start (1+ i)))
      ((>= i end) seq)
    (stream-write-char stream (elt seq i))))

(defmethod stream-write-sequence ((stream fundamental-binary-output-stream) seq
                                  &optional (start 0) (end (length seq)))
  (do ((i start (1+ i)))
      ((>= i end) seq)
    (stream-write-byte stream (elt seq i))))

(provide "dotcl-gray")
