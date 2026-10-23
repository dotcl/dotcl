;;; contrib/dotcl-lock/resolve.lisp writes the lock; dotcl-lock loads it. The
;;; fake dist under package-units/resolve/dist holds rx 1.0.0, 1.4.0, 2.0.0,
;;; 2.1.0 and 3.0.0. rapp depends on ra (rx ^1.2), rd (rx ^1.0) and rb
;;; (rx >= 2.0 < 3). ra and rd share rx 1.4.0, loaded globally; rx 2.1.0 and
;;; rb go to the unit rx@2; rcommon, which every rx uses, is loaded once.

(require "asdf")
(require "dotcl-lock")
(load "contrib/dotcl-lock/resolve.lisp")

(defvar *pures-dir*
  (merge-pathnames "test/regression/package-units/resolve/" (truename "./")))
(defvar *pures-lock*
  (concatenate 'string (regression-temp-dir) "/pures/dotcl-systems.lock"))
(defvar *pures-translations*
  (symbol-value (uiop:find-symbol* "*OUTPUT-TRANSLATIONS*" "ASDF/OUTPUT-TRANSLATIONS")))

(defun pures-sources ()
  (list (merge-pathnames "app/" *pures-dir*) (merge-pathnames "dist/" *pures-dir*)))

(dotcl-lock-resolve:resolve '("rapp") (pures-sources) :output *pures-lock*)

(defvar *pures-read*
  (with-open-file (in *pures-lock*)
    (with-standard-io-syntax (let ((*read-eval* nil)) (cdr (read in))))))

(defun pures-entry-summary (entry)
  (list (first entry)
        (getf (rest entry) :version)
        (getf (rest entry) :unit)
        (getf (rest entry) :boundary)
        (enough-namestring (getf (rest entry) :asd) *pures-dir*)))

(deftest package-units-resolve.lock-systems
  (sort (mapcar #'pures-entry-summary (getf *pures-read* :systems))
        #'string< :key (lambda (e) (format nil "~{~A~^ ~}" e)))
  (("ra" "1.0" nil nil "dist/ra-1.0/ra.asd")
   ("rapp" nil nil nil "app/rapp.asd")
   ("rb" "1.0" "rx@2" t "dist/rb-1.0/rb.asd")
   ("rcommon" "1.0" nil nil "dist/rcommon-1.0/rcommon.asd")
   ("rd" "1.0" nil nil "dist/rd-1.0/rd.asd")
   ("rx" "1.4.0" nil nil "dist/rx-1.4.0/rx.asd")
   ("rx" "2.1.0" "rx@2" nil "dist/rx-2.1.0/rx.asd")))

(deftest package-units-resolve.lock-units
  (mapcar (lambda (u) (list (getf u :name) (getf u :packages) (getf u :systems)))
          (getf *pures-read* :units))
  (("rx@2" ("RX" "RX/EXTRA") ("rx"))))

;;; rx may have one version only: ra's ^1.2 and rb's >= 2.0 < 3 cannot meet.
(deftest package-units-resolve.single-conflict
  (handler-case (progn (dotcl-lock-resolve:resolve '("rapp") (pures-sources) :single '("rx")) nil)
    (dotcl-lock-resolve:resolve-failed (c)
      (list (dotcl-lock-resolve:resolve-failed-system c)
            (mapcar #'second (dotcl-lock-resolve:resolve-failed-requirements c)))))
  ("rx" ((:^ "1.2") (:>= "2.0" :< "3"))))

(asdf:initialize-output-translations
 `(:output-translations
   (t (,(concatenate 'string (regression-temp-dir) "/pures-fasl/") :**/ :*.*.*))
   :ignore-inherited-configuration))
(dotcl-lock:use-lock *pures-lock*)
(asdf:load-system "rapp")

(deftest package-units-resolve.two-versions
  (funcall (read-from-string "rapp:run"))
  ("1.4.0" "1.4.0" "2.1.0" (:bye "rb")))

(deftest package-units-resolve.shared-once
  (list (symbol-value (find-symbol "*LOADS*" "RCOMMON"))
        (sort (copy-list (symbol-value (find-symbol "*NOTES*" "RCOMMON"))) #'string<))
  (1 ("rx 1.4.0" "rx 2.1.0")))

(deftest package-units-resolve.unit-packages
  (list (find-symbol "FAREWELL" "RX")
        (and (find-package "RX@2!RX") (find-package "RX@2!RX/EXTRA") t)
        (find-package "RX/EXTRA")
        (dotcl-lock:call-in-unit "rx@2"
          (lambda () (asdf:component-version (asdf:find-system "rx")))))
  (nil t nil "2.1.0"))

(setf asdf:*system-definition-search-functions*
      (remove 'dotcl-lock::sysdef-lock-search asdf:*system-definition-search-functions*)
      dotcl-lock::*lock* nil
      (symbol-value (uiop:find-symbol* "*OUTPUT-TRANSLATIONS*" "ASDF/OUTPUT-TRANSLATIONS"))
      *pures-translations*)
