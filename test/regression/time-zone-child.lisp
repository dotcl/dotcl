;;; regression: skip-under-interp
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

(deftest time-zone-child.fall-back-overlap-daylight
  (tzc-values "America/New_York" "(decode-universal-time 3939602399)")
  59 59 1 3 11 2024 6 t 5)

(deftest time-zone-child.fall-back-overlap-standard
  (tzc-values "America/New_York" "(decode-universal-time 3939602400)")
  0 0 1 3 11 2024 6 nil 5)

(deftest time-zone-child.fall-back-overlap-central-daylight
  (tzc-values "America/Chicago" "(decode-universal-time 3939604230)")
  30 30 1 3 11 2024 6 t 6)

(deftest time-zone-child.fall-back-overlap-central-standard
  (tzc-values "America/Chicago" "(decode-universal-time 3939607830)")
  30 30 1 3 11 2024 6 nil 6)

(deftest time-zone-child.spring-forward-before
  (tzc-values "America/New_York" "(decode-universal-time 3919042799)")
  59 59 1 10 3 2024 6 nil 5)

(deftest time-zone-child.spring-forward-after
  (tzc-values "America/New_York" "(decode-universal-time 3919042800)")
  0 0 3 10 3 2024 6 t 5)

(deftest time-zone-child.explicit-zone-not-local
  (tzc-values "America/New_York" "(decode-universal-time 3939602399 0)")
  59 59 5 3 11 2024 6 nil 0)

) ; #-windows
