;;; A fasl compiled with a source path map records /dotcl-src/NAME/... instead of the directory the
;;; checkout was in. Where that library's source is on this machine (REGISTER-SOURCE-ROOT) the
;;; definition lookups turn the recorded path back into the real file; without it the path
;;; comes back as recorded.

(defun %spu-write (path text)
  (ensure-directories-exist path)
  (with-open-file (o path :direction :output :if-exists :supersede)
    (write-string text o))
  path)

(deftest-emitting-only source-path-unmap.finds-registered-source
  (let* ((base (format nil "~A/spu1" (regression-temp-dir)))
         (build (format nil "~A/build" base))
         (installed (format nil "~A/installed" base))
         (text "(defun spu-probe-fn (x) (1+ x))
")
         (src (%spu-write (format nil "~A/probe.lisp" build) text))
         (fasl (format nil "~A/probe.fasl" build)))
    (%spu-write (format nil "~A/probe.lisp" installed) text)
    (let ((dotcl:*source-path-map* (list (cons build "/dotcl-src/spu-lib"))))
      (compile-file src :output-file fasl))
    (load fasl)
    (let ((before (getf (dotcl:function-source-location 'spu-probe-fn) :file)))
      (dotcl:register-source-root "spu-lib" installed)
      (let ((after (getf (dotcl:function-source-location 'spu-probe-fn) :file))
            (listed (getf (first (dotcl:definition-sources 'spu-probe-fn)) :file)))
        (list before
              (and (search "/installed/probe.lisp" after) t)
              (equal after listed)
              (and (probe-file after) t))))
    )
  ("/dotcl-src/spu-lib/probe.lisp" t t t))
