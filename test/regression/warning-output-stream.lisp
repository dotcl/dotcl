;;; An unhandled WARNING is reported on *ERROR-OUTPUT* whatever kind of
;;; stream it is, not only a file or string stream.

(deftest warning-output-stream.string
  (with-output-to-string (*error-output*) (warn "wos-1"))
  "WARNING: wos-1
")

(deftest warning-output-stream.broadcast
  (with-output-to-string (s)
    (let ((*error-output* (make-broadcast-stream s)))
      (warn "wos-2")))
  "WARNING: wos-2
")

(deftest warning-output-stream.synonym
  (with-output-to-string (s)
    (let ((*error-output* (make-synonym-stream '*standard-output*))
          (*standard-output* s))
      (warn "wos-3")))
  "WARNING: wos-3
")
