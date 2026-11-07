;;; A relative :output / :input of UIOP:RUN-PROGRAM is made absolute against
;;; *DEFAULT-PATHNAME-DEFAULTS* (CLHS 19.2.3): the file the child's shell redirects to is the one the
;;; parent opened, not one in the process's own directory.
(require "asdf")

#-windows
(deftest run-program-relative-files.output-and-input
  (let* ((dir (format nil "~A/rp-test/" (regression-temp-dir))))
    (ensure-directories-exist dir)
    (let ((*default-pathname-defaults* (pathname dir)))
      (uiop:run-program '("echo" "hello") :output "rp-out.txt" :error-output nil)
      (uiop:run-program '("cat") :input "rp-out.txt" :output #p"rp-out2.txt" :error-output nil))
    (list (with-open-file (s (format nil "~Arp-out.txt" dir)) (read-line s nil))
          (with-open-file (s (format nil "~Arp-out2.txt" dir)) (read-line s nil))
          (null (probe-file "rp-out.txt"))))
  ("hello" "hello" t))
