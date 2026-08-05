;;; compile-file finally-block ASDF registry invalidation.
;;;
;;; The compile-file finally strip nulls Function slots of symbols newly
;;; fbound during the compile (e.g. a contrib loaded by a compile-time
;;; (require "x") via asdf:module-provide-asdf), and restores *modules* so
;;; the compile-time require is transient. But it did NOT clear ASDF's
;;; *registered-systems*: the require registered the system and set its
;;; load-op stamp (component-loaded-p → T). A load-time re-(require "x")
;;; then ran module-provide-asdf → find-system returned the stale stamped
;;; system → component-loaded-p was T → load-system no-op'd → the fasl was
;;; never reloaded → the stripped functions stayed unbound → callers
;;; signaled UNDEFINED-FUNCTION. The fix calls asdf:clear-system for each
;;; module added to *modules* during the compile, so the load-time require
;;; rebuilds an unstamped system and actually reloads the fasl.
;;;
;;; This is the load-bearing half of the fix (the cold-cache float-features
;;; failure). The cache-invalidation half (Startup._symFnCache) is defensive
;;; and is exercised end-to-end by the cold-cache float-features repro
;;; alongside this test.
;;;
;;; Hermetic: writes a temp .asd + source into a temp dir and pushes it to
;;; asdf:*central-registry* (the i390-test pattern). asdf symbols are reached
;;; via read-from-string so loading this file does not require asdf up front.

(defpackage #:cfri-mod (:use #:cl) (:export #:fn))

(defun %cfri-asdf-stale-registry ()
  (require "asdf")
  (let* ((tmp (format nil "~a/dotcl-cfri-~a/"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (dirp (substitute #\/ #\\ tmp))
         (mod-asd (concatenate 'string dirp "cfri-mod.asd"))
         (mod-src (concatenate 'string dirp "cfri-mod.lisp"))
         (drv-src (concatenate 'string dirp "cfri-drv.lisp"))
         (drv-fasl (concatenate 'string dirp "cfri-drv.fasl")))
    (ensure-directories-exist dirp)
    ;; The contrib system: one file defining cfri-mod:fn.
    (with-open-file (s mod-asd :direction :output :if-exists :supersede)
      (write-string "(defsystem \"cfri-mod\" :components ((:file \"cfri-mod\")))" s))
    (with-open-file (s mod-src :direction :output :if-exists :supersede)
      (write-string "(in-package #:cfri-mod) (defun fn () :loaded)" s))
    ;; Register the temp dir so asdf can find cfri-mod.
    (eval (read-from-string
           (format nil "(pushnew ~s asdf:*central-registry* :test #'equal)" dirp)))
    ;; Pre-clean any prior registration/state so the test is deterministic
    ;; across re-runs in the same image.
    (fmakunbound 'cfri-mod:fn)
    (let ((cs (find-symbol "CLEAR-SYSTEM" "ASDF")))
      (when cs (funcall cs "cfri-mod")))
    ;; Driver: a compile-time (require "cfri-mod") loads the contrib during
    ;; compile (module-provide-asdf → load-system → registers cfri-mod in
    ;; *registered-systems* with load-op stamp set; cfri-mod:fn becomes fbound).
    ;; The finally strip nulls cfri-mod:fn.Function (newly fbound this compile)
    ;; and restores *modules*. WITHOUT the fix, the stale *registered-systems*
    ;; entry survives, so the load-time require below no-ops and cfri-mod:fn
    ;; stays unbound. WITH the fix, clear-system "cfri-mod" removes the stale
    ;; entry, so the load-time require reloads the fasl and cfri-mod:fn is fbound.
    (with-open-file (s drv-src :direction :output :if-exists :supersede)
      (write-string "(eval-when (:compile-toplevel) (require \"cfri-mod\"))" s))
    (compile-file drv-src :output-file drv-fasl)
    ;; The strip already nulled cfri-mod:fn during compile; fmakunbound is
    ;; belt-and-suspenders to make the post-compile state unambiguous.
    (fmakunbound 'cfri-mod:fn)
    ;; Load-time re-require: the load-bearing step. With the fix this
    ;; reloads the fasl; without it, load-system no-ops on the stale registry.
    (require "cfri-mod")
    (list (fboundp 'cfri-mod:fn)
          (when (fboundp 'cfri-mod:fn) (funcall (symbol-function 'cfri-mod:fn))))))

(deftest compile-file-clears-asdf-registry-for-compile-time-require
  (%cfri-asdf-stale-registry)
  (t :loaded))
