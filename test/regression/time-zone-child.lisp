;;; Local-zone behaviour under TZ in a child process; TZ only works on Unix.
#-windows
(progn

(defvar *tzc-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *tzc-core* (regression-child-core))

(defun tzc-environment (zone)
  (cons (concatenate 'string "TZ=" zone)
        (remove-if (lambda (entry)
                     (and (>= (length entry) 3)
                          (string= entry "TZ=" :end1 3)))
                   (dotcl:posix-environ))))

(defun tzc-run (zone form)
  (let* ((p (dotcl:launch-process *tzc-exe*
                                  (list "--core" *tzc-core* "--eval" form)
                                  :environment (tzc-environment zone)
                                  :error :output))
         (out (with-output-to-string (s)
                (loop for line = (read-line (dotcl:process-output p) nil)
                      while line do (write-line line s)))))
    (dotcl:process-wait p)
    out))

(defun tzc-values (zone form)
  (let* ((text (concatenate 'string
                            "(format t \"RESULT=~S\" (multiple-value-list " form "))"))
         (out (tzc-run zone text))
         (pos (search "RESULT=" out)))
    (values-list (read-from-string out t nil :start (+ pos 7)))))

(deftest time-zone-child.utc-decode
  (tzc-values "UTC" "(decode-universal-time 3900000000)")
  0 20 21 2 8 2023 2 nil 0)

(deftest time-zone-child.utc-encode-base
  (tzc-values "UTC" "(encode-universal-time 0 0 0 1 1 1900)")
  0)

(deftest time-zone-child.utc-encode-before-base
  (tzc-values "UTC"
              "(handler-case (progn (encode-universal-time 0 0 0 1 1 1900 -9) :no-error)
                            (type-error () :type-error))")
  :type-error)

(deftest time-zone-child.dst-decode-standard-zone
  (tzc-values "America/New_York"
              "(decode-universal-time (encode-universal-time 0 0 12 1 7 2024 0))")
  0 0 8 1 7 2024 0 t 5)

(deftest time-zone-child.dst-local-encode-before-base
  (tzc-values "Asia/Tokyo"
              "(handler-case (progn (encode-universal-time 0 0 0 1 1 1900) :no-error)
                            (type-error () :type-error))")
  :type-error)

) ; #-windows
