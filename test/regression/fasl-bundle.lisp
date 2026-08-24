;;; Monolithic FASL bundles (uiop bundle-op, via dotcl:combine-fasls).
;;;
;;; Most implementations combine FASLs by concatenating object files. A dotcl
;;; FASL is a .NET PE assembly, and two of those concatenated are not an
;;; assembly, so combine-fasls used to signal "not supported" and bundle-op was
;;; simply unavailable here.
;;;
;;; A bundle is a container instead: an assembly carrying each input as a
;;; managed resource, which LOAD unpacks in order. What the contract requires is
;;; tested below -- one output file, loading it loads everything, in bundled
;;; order, and the inputs are no longer needed afterwards.

(defun %fb-tmpdir ()
  (let ((dir (format nil "~a/dotcl-fasl-bundle-~a/"
                     (or (dotcl:getenv "TEMP") "/tmp")
                     (get-internal-real-time))))
    (ensure-directories-exist dir)
    dir))

(defun %fb-write (path &rest lines)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (dolist (l lines) (write-line l s))))

(defun %fb-build-bundle (dir)
  "Compile two dependent files into DIR, bundle them, and delete the parts.
Returns the bundle's pathname. The second file calls a function from the first,
so a bundle that loses the order cannot load."
  (let ((a (format nil "~aa.lisp" dir))
        (b (format nil "~ab.lisp" dir)))
    (%fb-write a
               "(defun fb-part-a (x) (* x 2))"
               "(defparameter *fb-from-a* :a-loaded)")
    (%fb-write b
               "(defun fb-part-b (x) (+ (fb-part-a x) 1))"
               "(defparameter *fb-from-b* (list :b-loaded *fb-from-a*))")
    (let ((fa (compile-file a))
          (fb (compile-file b)))
      (let ((bundle (dotcl:combine-fasls (list fa fb) (format nil "~aall.fasl" dir))))
        ;; Delete the inputs: nothing may reload them from disk behind our back.
        (delete-file fa)
        (delete-file fb)
        bundle))))

(deftest fasl-bundle-loads-every-part
  (let* ((dir (%fb-tmpdir))
         (bundle (%fb-build-bundle dir)))
    (load bundle)
    (list (funcall (read-from-string "fb-part-a") 21)
          (funcall (read-from-string "fb-part-b") 10)))
  (42 21))

(deftest fasl-bundle-keeps-load-order
  ;; *fb-from-b* is built from *fb-from-a*, so this value exists only if the
  ;; parts ran in the order they were bundled.
  (symbol-value (read-from-string "*fb-from-b*"))
  (:b-loaded :a-loaded))

(deftest fasl-bundle-is-one-file
  (let* ((dir (%fb-tmpdir))
         (bundle (%fb-build-bundle dir)))
    (and (probe-file bundle) t))
  t)

;;; A bundle of one is still a bundle: the container shape must not depend on
;;; having several inputs.
(deftest fasl-bundle-single-input
  (let* ((dir (%fb-tmpdir))
         (src (format nil "~asolo.lisp" dir)))
    (%fb-write src "(defun fb-solo () :solo)")
    (let* ((fasl (compile-file src))
           (bundle (dotcl:combine-fasls (list fasl) (format nil "~asolo-bundle.fasl" dir))))
      (delete-file fasl)
      (load bundle)
      (funcall (read-from-string "fb-solo"))))
  :solo)

;;; A missing input is a FILE-ERROR naming the file, not a raw .NET exception.
(deftest fasl-bundle-missing-input-signals-file-error
  (let ((dir (%fb-tmpdir)))
    (handler-case
        (progn (dotcl:combine-fasls (list (format nil "~anope.fasl" dir))
                                    (format nil "~aout.fasl" dir))
               :no-error)
      (file-error (e) (and (search "does not exist" (princ-to-string e)) t))
      (error () :wrong-class)))
  t)
