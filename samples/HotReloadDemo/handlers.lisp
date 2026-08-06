;;;; The hot-reloadable part of the app. Edit this file while the host is
;;;; running; every save is re-loaded and the next request sees the change.
;;;;
;;;; Things to try while it runs:
;;;;   - change the greeting text
;;;;   - change the arithmetic (e.g. report (* n n) instead of n)
;;;;   - introduce a typo, save, and watch the host survive with the old
;;;;     definition, then fix it

(defun handle-request (n)
  (format nil "hello from Lisp, request #~a" n))
