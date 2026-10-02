;;; DOTNET:FIND-SYMBOL-ANY looked only in libraries opened through
;;; DOTNET:LOAD-LIBRARY, so a C library function named with no library --
;;; CFFI's (foreign-funcall "clock_gettime" ...), which precise-time does on
;;; macOS and Linux -- was "not found in any loaded library". It now also looks
;;; in the process, the way dlsym(RTLD_DEFAULT, ...) and SBCL do, and the C
;;; library is already loaded there. Unix only: on Windows the C runtime is not
;;; part of the main program's exports.

#-windows
(deftest ffi-process-symbols.libc-without-load-library
  (let ((p (dotnet:find-symbol-any "labs")))
    (and p (dotnet:%ffi-call-ptr p '(:int64) :int64 -42)))
  42)

(deftest ffi-process-symbols.unknown-name-is-nil
  (dotnet:find-symbol-any "dotcl_no_such_symbol_anywhere_4711")
  nil)
