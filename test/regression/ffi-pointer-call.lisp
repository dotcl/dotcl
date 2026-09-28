;;; The call path of a :pointer argument takes the same union as memory does
;;; (see ffi-pointer-range.lisp): -1 and #xFFFFFFFFFFFFFFFF are one address.
;;;
;;; The callee is a Lisp callback reached through %FFI-CALL-PTR, which is the
;;; path C takes and needs no C runtime library, so this runs on every
;;; platform. It hands back the pointer bits it received, so the value is seen
;;; by the argument converter on the way in and by the return converter on the
;;; way out. This is the cl-sqlite SQLITE_TRANSIENT shape: a destructor argument
;;; spelled (mod -1 (expt 2 64)).

(defun %ptr-identity-call (v)
  (dotnet:%ffi-call-ptr
   (dotnet:make-ffi-callback (lambda (p) p) '(:pointer) :pointer)
   '(:pointer) :pointer v))

(deftest ffi-pointer-call.both-spellings-reach-the-callee
  (list (%ptr-identity-call (mod -1 (expt 2 64)))
        (%ptr-identity-call -1)
        (%ptr-identity-call 0))
  ;; Pointers come back in the unsigned spelling, whichever one went in.
  (18446744073709551615 18446744073709551615 0))

;;; A callback may return the unsigned spelling too: its result goes through
;;; the same converter.
(deftest ffi-pointer-call.callback-returns-unsigned-spelling
  (dotnet:%ffi-call-ptr
   (dotnet:make-ffi-callback (lambda () (mod -1 (expt 2 64))) '() :pointer)
   '() :pointer)
  18446744073709551615)

(deftest ffi-pointer-call.out-of-range-signals
  (list (handler-case (progn (%ptr-identity-call (expt 2 64)) nil)
          (error () t))
        (handler-case (progn (%ptr-identity-call (- (1+ (expt 2 63)))) nil)
          (error () t)))
  (t t))
