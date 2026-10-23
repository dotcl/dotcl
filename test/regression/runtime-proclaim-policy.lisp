;;; The OPTIMIZE policy the running image proclaimed (by PROCLAIM at the REPL,
;;; or a load-time PROCLAIM in the fasl of another file) is the starting policy
;;; of a later COMPILE-FILE, as on SBCL 2.6.8: the file's functions get the
;;; proclaimed SAFETY and DEBUG unless the file declaims otherwise, an in-file
;;; DECLAIM changes nothing outside that file, and COMPILE-FILE leaves the
;;; image's policy as it was. The proclamation used to reach only the CLtL2
;;; record (*proclaimed-policy*), which no codegen decision consults.

(setf dotcl:*save-sil* t)

(defun %rpp-write (name lines)
  (let ((src (regression-temp-file (format nil "~a.lisp" name))))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    src))

;;; Compile LINES and answer the text SIL the same COMPILE-FILE left beside it.
(defun %rpp-compile-sil (name lines)
  (let* ((src (%rpp-write name lines))
         (out (regression-temp-file (format nil "~a.fasl" name))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file out :sil t))
    (regression-temp-file (format nil "~a.sil" name))))

(defun %rpp-sil-text (path)
  (with-open-file (s path)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

(defun %rpp-policy ()
  (list dotcl-internal::*optimize-safety*
        dotcl-internal::*optimize-debug*
        dotcl-internal::*proclaimed-policy*))

(deftest-compiled-only runtime-proclaim.compile-file-keeps-image-policy
  (let ((dotcl-internal::*optimize-safety* 1)
        (dotcl-internal::*optimize-debug* 1)
        (dotcl-internal::*proclaimed-policy* '()))
    (proclaim '(optimize (debug 0) (safety 0)))
    (when (%rpp-compile-sil
           "rpp1"
           '("(defun %rppd-f (n) (let ((s 0)) (dotimes (i n s) (setq s (+ s i)))))"))
      (%rpp-policy)))
  (0 0 ((safety . 0) (debug . 0))))

(deftest-compiled-only runtime-proclaim.file-gets-image-safety-and-debug
  (let ((dotcl-internal::*optimize-safety* 1)
        (dotcl-internal::*optimize-debug* 1)
        (dotcl-internal::*proclaimed-policy* '()))
    (proclaim '(optimize (debug 0) (safety 0)))
    (let ((txt (%rpp-sil-text
                (%rpp-compile-sil
                 "rpp2"
                 '("(defun %rppd-f (n) (let ((s 0)) (dotimes (i n s) (setq s (+ s i)))))")))))
      ;; Safety 0: no interrupt poll per back-edge; debug 0: the tail marker
      ;; carries no frame name, and carries the name for no compiled function.
      (list (and (search "PollInterrupt" txt) t)
            (and (search "TAIL-SELF NIL" txt) t)
            (and (search "TAIL-SELF %RPPTD-F" txt) t))))
  (nil t nil))

(deftest-compiled-only runtime-proclaim.file-declaim-wins-in-file
  (let ((dotcl-internal::*optimize-safety* 1)
        (dotcl-internal::*optimize-debug* 1)
        (dotcl-internal::*proclaimed-policy* '()))
    (proclaim '(optimize (debug 0) (safety 0)))
    (let ((txt (%rpp-sil-text
                (%rpp-compile-sil
                 "rpp3"
                 '("(declaim (optimize (safety 1) (debug 3)))"
                   "(defun %rppd-g (n) (let ((s 0)) (dotimes (i n s) (setq s (+ s i)))))")))))
      ;; The file's own DECLAIM overrides the image for the rest of the file.
      (list (and (search "PollInterrupt" txt) t)
            (and (search "TAIL-SELF" txt) t))))
  (t nil))

(deftest-compiled-only runtime-proclaim.file-declaim-stays-in-file
  (let ((dotcl-internal::*optimize-safety* 1)
        (dotcl-internal::*optimize-debug* 1)
        (dotcl-internal::*proclaimed-policy* '()))
    (proclaim '(optimize (debug 0) (safety 0)))
    (%rpp-compile-sil
     "rpp4"
     '("(declaim (optimize (safety 1) (debug 3)))"
       "(defun %rppd-g (n) (let ((s 0)) (dotimes (i n s) (setq s (+ s i)))))"))
    (eval '(defun %rppd-h (n) (let ((s 0)) (dotimes (i n s) (setq s (+ s i))))))
    ;; The image still holds what PROCLAIM said, not what the file DECLAIMed,
    ;; and a function defined after the COMPILE-FILE compiles under it.
    (list (%rpp-policy)
          (and (search "PollInterrupt"
                       (%flat-text (dotcl:function-sil #'%rppd-h)))
               t)))
  ((0 0 ((safety . 0) (debug . 0))) nil))

(deftest-compiled-only runtime-proclaim.compile-picks-up-debug
  (let ((dotcl-internal::*optimize-safety* 1)
        (dotcl-internal::*optimize-debug* 1)
        (dotcl-internal::*proclaimed-policy* '()))
    (proclaim '(optimize (debug 3)))
    (eval '(defun %rppd-tco (n) (if (eql n 0) 0 (%rppd-tco (1- n)))))
    ;; Debug 3 makes even its own calls ordinary, so the :tail-self marker is
    ;; gone (see tail-calls-off-p).
    (and (search "TAIL-SELF"
                 (%flat-text (dotcl:function-sil #'%rppd-tco)))
         t))
  nil)
