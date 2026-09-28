;;; An abandoned capture-lifting speculation must leave no slot behind.
;;;
;;; The compiler first tries to compile a local function that closes over a
;;; variable by passing that variable as an extra argument instead, which drops
;;; the closure allocation. A call site where the name no longer resolves to the
;;; SLOT the definition was written against -- an inner binding of the same name
;;; shadows it -- abandons the attempt by unwinding, and the form is recompiled
;;; the ordinary way.
;;;
;;; The unwind used to leak the abandoned attempt's compile state, so the retry
;;; ran with the first attempt's slot table still in scope and emitted a load of
;;; a slot it had never declared. In-process assembly accepted that silently;
;;; the FASL assembler checks, so the failure surfaced only through COMPILE-FILE
;;; -- which is how every quickloaded library is built:
;;;
;;;   [FaslAssembler] Error in AddTopLevelForm: Undeclared local: PATHNAME_368
;;;
;;; Hence the shape below: a parameter captured by an FLET, an inner LET* that
;;; rebinds that parameter's name, and the call inside a loop (whose body is a
;;; block, which is what puts the call site past the point of no return). It is
;;; pathname-utils' UNIX-NAMESTRING reduced to five lines.
;;;
;;; The values are checked in every build, but only a COMPILE-FILE build can
;;; catch the dangling load, so that half is DEFTEST-EMITTING-ONLY.

;;; ---- the shapes, evaluated ----

;; Minimal: capture, shadow, call from inside a loop.
(defun %las-min (x)
  (flet ((w (p) (list p x)))
    (let* ((x (list x)) (d (car x)))
      (loop for c in d do (w c))
      x)))

;; Two captures, and the shadowed one is re-derived from itself.
(defun %las-two (x y)
  (flet ((w (p) (list p x y)))
    (let* ((x (list x)) (d (car x)))
      (loop for c in d do (w c))
      y)))

;; The call site outside any loop keeps resolving to the ORIGINAL binding, which
;; is what the speculation is about: W must see the parameter, not the rebinding.
(defun %las-sees-outer-binding (x)
  (flet ((w () x))
    (let ((x :shadowed))
      (declare (ignorable x))
      (w))))

(deftest lift-abort-stale-local.values
  (list (%las-min '(1 2)) (%las-two '(1) :y) (%las-sees-outer-binding :outer))
  (((1 2)) :y :outer))

;;; ---- the shape that only COMPILE-FILE could catch ----

(defvar *las-dir*
  (let ((dir (concatenate 'string
                          (regression-temp-dir)
                          "/dotcl-las-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %las-compile-and-load (source name)
  (let ((lisp (concatenate 'string *las-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string source s))
    (load (compile-file lisp))
    t))

;; Before the fix this raised "Undeclared local: X_n" out of the FASL assembler.
;; The functions are named apart from the ones above so the fasl's definitions
;; are what the assertions call.
(deftest-emitting-only lift-abort-stale-local.compile-file-emits-declared-slots
  (progn
    (%las-compile-and-load
     "(in-package :cl-user)
      (defun %lasf-min (x)
        (flet ((w (p) (list p x)))
          (let* ((x (list x)) (d (car x)))
            (loop for c in d do (w c))
            x)))
      (defun %lasf-two (x y)
        (flet ((w (p) (list p x y)))
          (let* ((x (list x)) (d (car x)))
            (loop for c in d do (w c))
            y)))
      ;; pathname-utils' UNIX-NAMESTRING shape: &key parameters, a recursive
      ;; call in the other ETYPECASE branch, the captured parameter shadowed by
      ;; a LET* and the calls made from inside a LOOP.
      (defun %lasf-namestring (path &key (stream) junk-allowed)
        (etypecase stream
          (null (with-output-to-string (stream)
                  (%lasf-namestring path :stream stream :junk-allowed junk-allowed)))
          (stream
           (flet ((write-part (part)
                    (unless junk-allowed (write-string \"!\" stream))
                    (write-string part stream)))
             (let* ((path (list path)) (dir (car path)))
               (loop for component in dir do (write-part component))
               stream)))))"
     "las-shapes")
    (list (funcall (intern "%LASF-MIN") '(1 2))
          (funcall (intern "%LASF-TWO") '(1) :y)
          (funcall (intern "%LASF-NAMESTRING") '("a" "b"))))
  (((1 2)) :y "!a!b"))
