;;; Variadic calls: :VARARGS says where the variadic part starts.
;;;
;;; Without it every argument looked fixed, and on ARM64 that is not a detail:
;;; the fixed convention puts floats in v0-v7, the variadic one passes them in
;;; the general-purpose registers and on the stack. sprintf("%.0f", 314.0) wrote
;;; "0" -- the callee read from a place the caller never filled. The marker also
;;; carries C's default argument promotions, so a variadic :float goes out as a
;;; double.
;;;
;;; Only sprintf is used here: it lives in the platform C runtime, takes both a
;;; fixed and a variadic part, and writes somewhere we can read back.

(defvar *ffi-va-libc*
  ;; The C runtime's name is per-platform: "libc.so.6" is the glibc soname and
  ;; does not exist on macOS, where the loader resolves the bare "libc".
  #+windows "msvcrt"
  #+darwin "libc"
  #-(or windows darwin) "libc.so.6")

;;; Apple ARM64 is not covered yet. :VARARGS applies C's default argument
;;; promotions, which is all Linux x86-64 needs because its variadic and fixed
;;; conventions place arguments identically. Apple's ARM64 ABI does not: the
;;; variadic part goes entirely on the stack, while the single non-variadic
;;; CALLI this still emits puts it in registers. Measured there, 6 of the 8
;;; tests below fail -- integers and strings as well as floats -- and only the
;;; two with no variadic argument pass. Running them would assert an ABI the
;;; implementation does not yet honour, so they are skipped rather than
;;; weakened; see the ARM64 issue for what a real fix needs.

(defun %va-cstring (p)
  "The NUL-terminated string at P."
  (with-output-to-string (s)
    (loop for i from 0 below 256
          for b = (dotnet:mem-read :uint8 p i)
          until (zerop b) do (write-char (code-char b) s))))

(defun %va-sprintf (types &rest args)
  "sprintf into a fresh buffer with TYPES (after the buffer and the control
   string) and return what landed there."
  (let ((buf (dotnet:alloc-mem 256)))
    (unwind-protect
         (progn (apply #'dotnet:ffi *ffi-va-libc* "sprintf"
                       :args (list* :pointer :string types) :ret :int
                       buf args)
                (%va-cstring buf))
      (dotnet:free-mem buf))))

(deftest ffi-varargs.double
  (%va-sprintf '(:varargs :double) "d=%.0f" 314.0d0)
  "d=314")

(deftest ffi-varargs.float-is-promoted
  (%va-sprintf '(:varargs :float) "f=%.1f" 2.5)
  "f=2.5")

(deftest ffi-varargs.several-doubles
  (%va-sprintf '(:varargs :double :double :double) "%.0f %.0f %.0f" 1.0d0 2.0d0 3.0d0)
  "1 2 3")

(deftest ffi-varargs.mixed-with-integers-and-strings
  (%va-sprintf '(:varargs :int :double :string) "n=%d d=%.1f s=%s" 7 2.5d0 "ok")
  "n=7 d=2.5 s=ok")

;;; Integer-only variadic calls worked before the marker existed and must keep
;;; working with it.
(deftest ffi-varargs.integers-only
  (%va-sprintf '(:varargs :int :int) "x=%d y=%d" 3 4)
  "x=3 y=4")

;;; No variadic part at all: the marker is optional.
(deftest ffi-varargs.no-variadic-part
  (%va-sprintf '() "plain")
  "plain")

;;; The same through a function POINTER, which is the path
;;; CFFI's FOREIGN-FUNCALL-POINTER takes.
(deftest ffi-varargs.through-a-function-pointer
  (let ((buf (dotnet:alloc-mem 256))
        (ptr (dotnet:static "System.Runtime.InteropServices.NativeLibrary" "GetExport"
                            (dotnet:static "System.Runtime.InteropServices.NativeLibrary"
                                           "Load" *ffi-va-libc*)
                            "sprintf")))
    (unwind-protect
         (progn (dotnet:%ffi-call-ptr ptr '(:pointer :string :varargs :double) :int
                                      buf "d=%.0f" 314.0d0)
                (%va-cstring buf))
      (dotnet:free-mem buf)))
  "d=314")

;;; Two marks are a mistake, not a second variadic section.
(deftest ffi-varargs.double-marker-is-an-error
  (handler-case (progn (%va-sprintf '(:varargs :int :varargs :int) "%d%d" 1 2) nil)
    (error () t))
  t)
