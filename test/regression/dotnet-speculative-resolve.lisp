;;; An internal guess must not run the caller's handlers, and a bare CATCH must
;;; not eat a handler's escape.
;;;
;;; dotnet:make-generic-type accepts a name with or without its backtick-arity,
;;; so it tries the name as given before adding one. That attempt used to signal
;;; when the bare name did not resolve -- running the caller's HANDLER-BIND for a
;;; lookup the caller never asked for -- and the wrapper around it was written
;;; `try { ... } catch { return null; }`. A bare catch takes everything, so a
;;; handler that answered with RETURN-FROM had its transfer of control swallowed:
;;; whatever the handler had already done stood, while the escape did not happen.
;;;
;;; usocket's suite showed the consequence. Its wait-for-input asks for
;;; List<Socket> by the bare name, so RT's error handler ran mid-test, set the
;;; "aborted" flag and tried to leave -- the leaving vanished, the test body ran
;;; on to the right answer, and RT reported a test that had aborted while showing
;;; the correct value.

(defvar *dsr-ran* nil)

;;; The speculative lookup is silent, and the call still answers.
(deftest dsr-internal-guess-runs-no-handler
  (let ((*dsr-ran* nil))
    (let ((result (block probe
                    (handler-bind ((error (lambda (condition)
                                            (declare (ignore condition))
                                            (setf *dsr-ran* t)
                                            (return-from probe :handler-exited))))
                      (dotnet:make-generic-type "System.Collections.Generic.List"
                                                (list "System.Int32"))))))
      (list *dsr-ran*
            (and (search "List`1" (dotnet:invoke result "ToString")) t))))
  (nil t))

;;; A real failure still signals, and the handler's escape still works.
(deftest dsr-real-failure-still-escapes
  (let ((*dsr-ran* nil))
    (list (block probe
            (handler-bind ((error (lambda (condition)
                                    (declare (ignore condition))
                                    (setf *dsr-ran* t)
                                    (return-from probe :handler-exited))))
              (dotnet:resolve-type "No.Such.Type.At.All")))
          *dsr-ran*))
  (:handler-exited t))

;;; handler-case sees it too, rather than the error being swallowed.
(deftest dsr-real-failure-is-catchable
  (handler-case (progn (dotnet:resolve-type "No.Such.Type.At.All") :no-error)
    (error () :error))
  :error)

;;; The arity-suffixed spelling was never speculative and still works.
(deftest dsr-explicit-arity-unchanged
  (and (search "Dictionary`2"
               (dotnet:invoke (dotnet:make-generic-type "System.Collections.Generic.Dictionary`2"
                                                        (list "System.String" "System.Int32"))
                              "ToString"))
       t)
  t)
