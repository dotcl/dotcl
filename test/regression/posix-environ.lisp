;;; DOTCL:POSIX-ENVIRON (as sb-ext:posix-environ).

(deftest posix-environ.shape
  (let ((env (dotcl:posix-environ)))
    (and (consp env)
         (every (lambda (e) (and (stringp e) (position #\= e))) env)
         t))
  t)

(deftest posix-environ.sees-setenv
  (progn
    (dotcl:setenv "DOTCL_POSIX_ENVIRON_TEST" "a=b c")
    (prog1 (list (find "DOTCL_POSIX_ENVIRON_TEST=a=b c" (dotcl:posix-environ) :test #'string=)
                 (progn (dotcl:unsetenv "DOTCL_POSIX_ENVIRON_TEST")
                        (find-if (lambda (e) (eql 0 (search "DOTCL_POSIX_ENVIRON_TEST=" e)))
                                 (dotcl:posix-environ))))))
  ("DOTCL_POSIX_ENVIRON_TEST=a=b c" nil))

(deftest posix-environ.passes-to-launch-process
  ;; Adding to the parent's environment, as external-program does.
  (let* ((env (cons "DOTCL_POSIX_ENVIRON_CHILD=42" (dotcl:posix-environ)))
         (out (make-string-output-stream))
         (p (if (member :windows dotcl:*host-features*)
                (dotcl:launch-process "cmd" '("/c" "echo %DOTCL_POSIX_ENVIRON_CHILD%")
                                      :environment env :output out :input nil :error nil)
                (dotcl:launch-process "/bin/sh" '("-c" "echo $DOTCL_POSIX_ENVIRON_CHILD")
                                      :environment env :output out :input nil :error nil))))
    (dotcl:process-wait p)
    (string-trim '(#\Space #\Return #\Newline) (get-output-stream-string out)))
  "42")
