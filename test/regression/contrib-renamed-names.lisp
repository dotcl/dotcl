;;; The four contribs that used to ship under an unprefixed name.
;;;
;;; advice / clrmd / decompiler / nuget are ordinary words, and dotcl pushes the
;;; contrib directories onto asdf:*central-registry*, which ASDF consults before
;;; the source-registry. A user with their own advice.asd therefore lost to ours
;;; silently -- no error, just a different system loaded. They now carry the
;;; dotcl- prefix the other bundled systems already had.
;;;
;;; Requiring an old name must not fail with a bare "not found": the message has
;;; to name the new module, because that is the only thing that tells a reader of
;;; an older article what to write instead. The hint is consulted only after
;;; every module provider has declined, so a user who really does have their own
;;; "nuget" system still gets theirs.

(deftest contrib-old-names-report-the-new-name
  (mapcar (lambda (old)
            (handler-case (progn (require old) :loaded)
              (error (e)
                (let ((text (format nil "~a" e)))
                  (if (search "renamed to" text) :says-renamed :bare-error)))))
          '("advice" "clrmd" "decompiler" "nuget"))
  (:says-renamed :says-renamed :says-renamed :says-renamed))

(deftest contrib-old-name-message-carries-the-replacement
  (let ((text (handler-case (progn (require "nuget") "")
                (error (e) (format nil "~a" e)))))
    (and (search "dotcl-nuget" text) t))
  t)

;;; A name that was never one of ours still gets the plain message -- the hint
;;; must not attach itself to every failed REQUIRE.

(deftest contrib-unknown-module-has-no-rename-hint
  (let ((text (handler-case (progn (require "no-such-module-xyzzy") "")
                (error (e) (format nil "~a" e)))))
    (and (search "not found" text) (not (search "renamed to" text))))
  t)
