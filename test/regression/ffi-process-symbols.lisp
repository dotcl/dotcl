;;; DOTNET:FIND-SYMBOL-ANY looked only in libraries opened through
;;; DOTNET:LOAD-LIBRARY, so a C library function named with no library --
;;; CFFI's (foreign-funcall "clock_gettime" ...), which precise-time does on
;;; macOS and Linux -- was "not found in any loaded library". It now also looks
;;; in the process, the way dlsym(RTLD_DEFAULT, ...) and SBCL do, and the C
;;; library is already loaded there. On Windows the main program exports
;;; nothing, so it looks through every module loaded in the process: kernel32
;;; for system-locale's GetUserDefaultLocaleName, the C runtime for labs.

(deftest ffi-process-symbols.libc-without-load-library
  (let ((p (dotnet:find-symbol-any "labs")))
    (and p (dotnet:%ffi-call-ptr p '(:int64) :int64 -42)))
  42)

(deftest ffi-process-symbols.unknown-name-is-nil
  (dotnet:find-symbol-any "dotcl_no_such_symbol_anywhere_4711")
  nil)

#+windows
(deftest ffi-process-symbols.kernel32-without-load-library
  (let ((p (dotnet:find-symbol-any "GetUserDefaultLocaleName"))
        (buf (dotnet:alloc-mem 170)))
    (unwind-protect
         (and p (plusp (dotnet:%ffi-call-ptr p '(:pointer :int) :int buf 85)))
      (dotnet:free-mem buf)))
  t)
