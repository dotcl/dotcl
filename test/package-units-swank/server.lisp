;;; Two versions of swank in one image, each listening on its own port.
;;; ORDER=units: both in package units. ORDER=global: the new one global.
(require "asdf")
(require "dotcl-lock")
(defvar *root* (dotcl:getenv "ROOT"))
(defun under (name) (concatenate 'string *root* "/" name))
(asdf:initialize-output-translations
 `(:output-translations (t (,(under "fasl/") :**/ :*.*.*)) :ignore-inherited-configuration))
(defun start (port)
  (asdf:load-system "swank")
  (uiop:symbol-call :swank :create-server :port port :dont-close t))
(cond ((string= (dotcl:getenv "ORDER") "global")
       (push (under "slime-new/") asdf:*central-registry*)
       (start 47472))
      (t
       (dotcl-lock:add-unit "swank@new" (under "slime-new/swank.asd"))
       (dotcl-lock:call-in-unit "swank@new" (lambda () (start 47472)))))
(dotcl-lock:add-unit "swank@231" (under "slime-231/swank.asd"))
(dotcl-lock:call-in-unit "swank@231" (lambda () (start 47471)))
(with-open-file (s (under "server.ready") :direction :output :if-exists :supersede)
  (print :ready s))
(loop repeat 600 do (sleep 1))
