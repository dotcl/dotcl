;;; Loaded last: no earlier test file may leave frame recording switched on.
;;;
;;; The flag is global and is read while a form is being compiled, so a file that
;;; turns it on for its own tests and forgets to turn it back off silently
;;; changes the code generated for every file loaded after it. That is invisible
;;; -- the tests still pass -- but the default codegen stops being exercised, and
;;; a loop's native slots start boxing their value into the frame on every store.
;;; A file that needs it on turns it off again when done.

(deftest frame-flag-default-is-off
  dotcl:*emit-frame-locals*
  nil)
