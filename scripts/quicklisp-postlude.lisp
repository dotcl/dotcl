;;;; dotcl postlude for the bundled quicklisp client. Emitted after the client's
;;;; own components by scripts/build-quicklisp.sh, and so runs when the contrib
;;;; fasl is loaded — i.e. at (require "quicklisp").
;;;;
;;;; Upstream this is the tail of the quicklisp home's setup.lisp, which ends by
;;;; calling (quicklisp:setup). dotcl cannot call it unconditionally: setup runs
;;;; maybe-initial-setup, which on a machine with no dist yet downloads and
;;;; installs one with :prompt nil. A (require "quicklisp") that silently pulls
;;;; tens of megabytes off the network is the wrong default, so the first-run
;;;; install is left to an explicit (ql:setup) by the user and this file only
;;;; says so.
;;;;
;;;; The flip side of that rule: (ql:setup) IS the user asking for the network,
;;;; so it installs the dotcl overlay dist too. Everything a dotcl user needs is
;;;; then reachable from the two things the printed messages already name —
;;;; (require "quicklisp") and (ql:setup) — with no third incantation to look up.
;;;;
;;;; Once a dist is present setup touches the network for nothing, so the normal
;;;; case — every run after the first — is wired up automatically.

(in-package #:ql-setup)

;;; https
;;;
;;; ql-http speaks plain HTTP over its own socket code and has no TLS, so
;;; anything served over https is unreachable with the stock client. The client
;;; does export the hook for it — *fetch-scheme-functions* dispatches FETCH on
;;; the URL scheme — so this is a registration, not a patch: the shipped client
;;; source stays identical to what was submitted upstream.

(defvar *https-credentials-function* nil
  "Called with a host name; returns an alist of (header-name . value) to send,
or NIL for none. Left unset, https requests are anonymous, which is what a
public dist needs. Setting it is how a private dist or an in-house server
becomes reachable — dotcl never embeds a token and never assumes a particular
host.")

(defun %credentials-for (host)
  (when *https-credentials-function*
    (funcall *https-credentials-function* host)))

(defun %url-host (url)
  "Host part of URL, for the credentials lookup."
  (let* ((mark (search "://" url))
         (start (if mark (+ mark 3) 0))
         (end (position-if (lambda (c) (member c '(#\/ #\? #\#))) url :start start))
         (authority (subseq url start end))
         (at (position #\@ authority)))
    (when at
      (setf authority (subseq authority (1+ at))))
    (let ((colon (position #\: authority)))
      (if colon (subseq authority 0 colon) authority))))

(defun https-fetch (url file &key (follow-redirects t) quietly
                               (if-exists :rename-and-delete)
                               (maximum-redirects ql-http:*maximum-redirects*))
  "FETCH scheme function for https. Signature and return values match
ql-http:http-fetch, so callers cannot tell the two apart."
  (declare (ignore if-exists))
  (setf file (merge-pathnames file))
  (let ((stream (if quietly (make-broadcast-stream) *trace-output*))
        ;; Download beside the target and move it into place only on success, so
        ;; a failed transfer cannot leave a truncated file where a good one was.
        (temp (merge-pathnames "https.tmp" file)))
    (format stream "~&; Fetching ~A~%" url)
    (ensure-directories-exist temp)
    (let ((status (dotcl::%http-fetch url
                                      (namestring temp)
                                      (%credentials-for (%url-host url))
                                      (if follow-redirects maximum-redirects 0))))
      (unless (<= 200 status 299)
        (ql-util:delete-file-if-exists temp)
        (error 'ql-http:unexpected-http-status :url url :status-code status))
      (ql-util:replace-file temp file)
      (format stream "; ~$KB~%" (/ (ql-util:file-size file) 1024))
      ;; ql-http:http-fetch returns (values header file); no caller in the client
      ;; uses the header, and there is no HTTP header object to hand back from
      ;; the .NET side, so the first value is NIL.
      (values nil (probe-file file)))))

(pushnew (cons "https" 'https-fetch) ql-http:*fetch-scheme-functions*
         :key #'car :test #'equal)

;;; the dotcl overlay dist
;;;
;;; A handful of libraries need dotcl-specific patches that have not reached the
;;; stock distribution yet. They are published as an ordinary quicklisp dist,
;;; given a higher preference than the stock one, so quickload picks the patched
;;; release for those few names and the stock release for everything else. The
;;; overlay shrinks as patches land upstream; an empty one is the goal.

(defvar *dotcl-dist-url* "https://dotcl.github.io/dist/dotcl.txt")

(defvar *offer-dotcl-dist* t
  "Whether dotcl offers the overlay dist at all: (ql:setup) installs it, and a
(require \"quicklisp\") that finds it missing says so. Set to NIL for a
stock-only quicklisp home — nothing is installed and nothing is printed.")

(defun dotcl-dist ()
  (ql-dist:find-dist "dotcl"))

(defun install-dotcl-dist ()
  "Install the overlay dist and give it priority over the stock one.

Run for you by (ql:setup); exported so it can be run by hand after a failed
network fetch, or after *OFFER-DOTCL-DIST* was turned back on.

Deliberately not run from (require :quicklisp): it touches the network, and a
require that quietly reaches out is the wrong default. Once installed nothing
further is needed — the preference is stored in the quicklisp home, so every
later session picks the patched releases without asking."
  (unless (dotcl-dist)
    (ql-dist:install-dist *dotcl-dist-url* :prompt nil))
  (let ((ours (dotcl-dist))
        (stock (ql-dist:find-dist "quicklisp")))
    (when (and ours stock)
      ;; Set once, at install time. Re-asserting it on every load would undo a
      ;; deliberate change by the user.
      (setf (ql-dist:preference ours) (+ 10 (ql-dist:preference stock))))
    ours))

(defun %report-missing-overlay (stream)
  (format stream
          "~&;; quicklisp: the dotcl overlay dist is not installed.~@
             ;; Run (ql-setup:install-dotcl-dist) for the dotcl-patched libraries,~@
             ;; or (setf ql-setup:*offer-dotcl-dist* nil) to stop seeing this.~%"))

;;; (ql:setup) — where the overlay gets installed
;;;
;;; SETUP is the point where the user has asked for the network: on a fresh home
;;; it downloads and installs the stock dist. Fetching the overlay in the same
;;; breath is the difference between "dotcl gives you the patched releases" and
;;; "dotcl gives you the patched releases once you find out about a second
;;; incantation". The constraint from the header — require must not touch the
;;; network — is untouched: the require path below calls the client's own SETUP
;;; directly, not this wrapper.
;;;
;;; The client's function is wrapped here rather than edited: the bundled branch
;;; stays byte-identical to what was submitted upstream, exactly as with the
;;; https fetch registration above.

(defvar *client-setup* (fdefinition 'quicklisp:setup)
  "The client's own SETUP, before the overlay wrapper below replaced it.")

(defun %setup-with-overlay (&rest args)
  "SETUP, plus the overlay dist. A failed overlay fetch is reported and dropped:
a home that reached the stock dist but not ours is still a working home, and
turning that into a failed SETUP would strand the user with no dist at all."
  (multiple-value-prog1 (apply *client-setup* args)
    (when (and *offer-dotcl-dist* (not (dotcl-dist)))
      (handler-case (install-dotcl-dist)
        (error (condition)
          (format *error-output*
                  "~&;; quicklisp: could not install the dotcl overlay dist: ~a~@
                     ;; Stock releases will be used. Retry with (ql-setup:install-dotcl-dist).~%"
                  condition))))))

(setf (fdefinition 'quicklisp:setup) #'%setup-with-overlay)

(defun %report-uninitialized-home (stream)
  (format stream
          "~&;; quicklisp: no dist installed under ~A~@
             ;; The first (ql:quickload ...) installs one from ~A,~@
             ;; or run (ql:setup) now to do it up front.~%"
          (namestring *quicklisp-home*)
          quicklisp::*initial-dist-url*))

;;; (ql:quickload) on a home with no dist — set it up rather than fail
;;;
;;; REQUIRE must not touch the network, so a fresh home has no dist when the
;;; client is loaded. QUICKLOAD is the other side of that: asking for a library
;;; by name IS the request to go and get it, so failing with "System X not
;;; found" reports the wrong thing — the system is fine, the home is empty.
;;;
;;; So the network moment moves from "the user reads a note and runs a second
;;; incantation" to "the first quickload takes a few seconds longer", which is
;;; where a user already expects to wait. SETUP up front still works and is
;;; still the way to pay that cost at a time you choose.
(defvar *client-quickload* (fdefinition 'quicklisp:quickload)
  "The client's own QUICKLOAD, before the auto-setup wrapper below replaced it.")

(defun %quickload-initializing-home (&rest args)
  "QUICKLOAD, installing a dist first if this home has none."
  (unless (quicklisp::dists-initialized-p)
    (format *error-output*
            "~&;; quicklisp: no dist under ~A yet — installing one first.~%"
            (namestring *quicklisp-home*))
    ;; The overlay wrapper, not the client's SETUP: a home initialized by
    ;; quickload should end up with the same dists as one initialized by hand.
    (quicklisp:setup))
  (apply *client-quickload* args))

(setf (fdefinition 'quicklisp:quickload) #'%quickload-initializing-home)

;; dists-initialized-p is the client's own predicate for "has a dist been
;; installed here", reused rather than reimplemented so the two cannot drift.
(if (quicklisp::dists-initialized-p)
    (progn
      ;; The client's own SETUP, not the overlay wrapper: with a dist already
      ;; present this touches the network for nothing, and require must keep it
      ;; that way. Installing the overlay is (ql:setup)'s job.
      (funcall *client-setup*)
      (when (and *offer-dotcl-dist* (not (dotcl-dist)))
        (%report-missing-overlay *error-output*)))
    (%report-uninitialized-home *error-output*))
