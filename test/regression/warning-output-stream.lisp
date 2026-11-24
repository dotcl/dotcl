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

;;; A STYLE-WARNING nobody handles is reported as one, so a log that looks for
;;; "WARNING:" finds only the warnings that are not about style.
(define-condition wos-style (style-warning) ()
  (:report (lambda (c s) (declare (ignore c)) (write-string "wos-4" s))))

(deftest warning-output-stream.style-warning-is-labelled
  (with-output-to-string (*error-output*) (warn 'wos-style))
  "STYLE-WARNING: wos-4
")

(deftest warning-output-stream.plain-warning-keeps-its-label
  (with-output-to-string (*error-output*) (warn "wos-5"))
  "WARNING: wos-5
")
