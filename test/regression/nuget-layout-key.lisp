;;; What NUGET keeps between processes, what it refuses to resolve on its own,
;;; and what `dotcl pack' carries beside an executable.
;;;
;;; Resolution is two steps on throwaway projects: a restore that decides the
;;; versions (NuGet's lock file is the answer, "the pins"), and a build that lays
;;; the pinned versions out for one RID. Laid-out pins are kept under a key that
;;; stands for exactly those versions, so a later process reuses them.
;;;
;;; Declared requests (a system definition's) follow the project's lock file,
;;; dotcl-nuget.lock.json: what it records is used without asking NuGet, an exact
;;; version it does not record is resolved and recorded, and a floating version it
;;; does not record is refused with a pointer to NUGET:RESTORE.
;;;
;;; Only decisions are tested here. Resolving for real needs the network and the
;;; .NET SDK, so lock files and layouts are faked, and every test that must not
;;; reach for either runs with DOTCL_NUGET_OFFLINE=1 -- which turns any attempt
;;; into an error, so a test that passes did not start `dotnet'.

(require "dotcl-nuget")

(defun nlk-new-dir (tag)
  (let ((d (nuget::%combine (regression-temp-dir)
                            (format nil "dotcl-nlk-~a-~a" tag
                                    (dotnet:invoke (dotnet:static "System.Guid" "NewGuid")
                                                   "ToString" "N")))))
    (dotnet:static "System.IO.Directory" "CreateDirectory" d)
    d))

(defun nlk-file (dir name text)
  (let ((p (nuget::%combine dir name)))
    (dotnet:static "System.IO.Directory" "CreateDirectory"
                   (dotnet:static "System.IO.Path" "GetDirectoryName" p))
    (dotnet:static "System.IO.File" "WriteAllText" p text)
    p))

(defun nlk-exists (path)
  (and (dotnet:static "System.IO.File" "Exists" path) t))

(defun nlk-message (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (error (e) (princ-to-string e))))

(defmacro with-nlk-sandbox ((&key (offline t)) &body body)
  "Run BODY with a fresh image set and its own project, cache and bundle
directories, so nothing here reads or writes the real ones."
  (let ((saved (gensym)))
    `(let ((nuget::*states* (make-hash-table :test #'equal))
           (nuget::*project-directory* (nlk-new-dir "proj"))
           (nuget::*cache-directory* (nlk-new-dir "cache"))
           (nuget::*bundle-directory* (nlk-new-dir "bundle"))
           (,saved (dotcl:getenv "DOTCL_NUGET_OFFLINE")))
       (dotcl:setenv "DOTCL_NUGET_OFFLINE" ,(if offline "1" ""))
       (unwind-protect (progn ,@body)
         (dotcl:setenv "DOTCL_NUGET_OFFLINE" (or ,saved ""))))))

(defun nlk-lock (directs transitives &optional (tfm (nuget::%current-tfm)))
  "Write a lock file into the sandbox project. DIRECTS are (id requested resolved),
TRANSITIVES (id resolved)."
  (nlk-file (namestring nuget::*project-directory*) "dotcl-nuget.lock.json"
            (format nil "{\"version\":1,\"dependencies\":{\"~A\":{~{~A~^,~}}}}"
                    tfm
                    (append
                     (loop for (id req res) in directs
                           collect (format nil "\"~A\":{\"type\":\"Direct\",\"requested\":\"~A\",\"resolved\":\"~A\",\"contentHash\":\"x\"}"
                                           id req res))
                     (loop for (id res) in transitives
                           collect (format nil "\"~A\":{\"type\":\"Transitive\",\"resolved\":\"~A\",\"contentHash\":\"x\"}"
                                           id res))))))

(defun nlk-fake-layout (pins &optional (rid (nuget::%current-rid)) (tfm (nuget::%current-tfm)))
  "Put a finished layout for PINS into the sandbox cache, as a build would."
  (let ((dir (nuget::%combine nuget::*cache-directory* (nuget::%layout-key pins rid tfm))))
    (nlk-file dir "Fake.Package.dll" "not really an assembly")
    (nuget::%write-marker dir rid tfm pins '())
    dir))

(defun nlk-req (id spec &optional (origin :declared))
  (nuget::make-req id spec nil origin))

;;; --- what is exact ---------------------------------------------------------

(deftest nlk-exact-versions-are-kept
  (mapcar #'nuget::%exact-version-p '("13.0.3" "1.0" "2.88.7-beta1"))
  (t t t))

(deftest nlk-floating-versions-are-not-kept
  (mapcar (lambda (v) (and (nuget::%exact-version-p v) t))
          '("*" "*-*" "13.*" "[1.0,2.0)" "(1.0,)" "1.0, 2.0"))
  (nil nil nil nil nil nil))

(deftest nlk-empty-version-is-not-kept
  (and (nuget::%exact-version-p "") t)
  nil)

;;; What NuGet writes as "requested" in a lock file, for the shapes compared
;;; without starting NuGet. A range gives NIL: NuGet's locked mode judges it.
(deftest nlk-normalize-spec
  (mapcar #'nuget::%normalize-spec
          '("13.0.3" "1.0" "2.88.7-beta1" "13.*" "*" "*-*" "[1.0,2.0)"))
  ("[13.0.3, )" "[1.0.0, )" "[2.88.7-beta1, )" "[13.*, )" "[*, )" "[*-*, )" nil))

;;; --- the key stands for exactly the versions -------------------------------

(defun nlk-key (&key (pins '(("P" . "1.0.0"))) (rid "win-x64") (tfm "net10.0"))
  (nuget::%layout-key pins rid tfm))

(deftest nlk-key-varies-with-each-axis
  (let ((base (nlk-key)))
    (list (equal base (nlk-key :pins '(("P" . "2.0.0"))))
          (equal base (nlk-key :pins '(("Q" . "1.0.0"))))
          (equal base (nlk-key :pins '(("P" . "1.0.0") ("Q" . "1.0.0"))))
          (equal base (nlk-key :rid "linux-arm64"))
          (equal base (nlk-key :tfm "net9.0"))))
  (nil nil nil nil nil))

;;; The same versions are the same key whatever order and case they come in, so
;;; a second process finds the first one's work.
(deftest nlk-key-is-stable
  (list (equal (nlk-key) (nlk-key))
        (equal (nlk-key :pins '(("A" . "1.0.0") ("b" . "2.0.0")))
               (nlk-key :pins '(("B" . "2.0.0") ("a" . "1.0.0")))))
  (t t))

(deftest nlk-key-is-a-safe-directory-name
  (and (every (lambda (c) (or (alphanumericp c) (find c "._-"))) (nlk-key)) t)
  t)

;;; --- where it goes ---------------------------------------------------------

(defun nlk-parent (path)
  (dotnet:static "System.IO.Path" "GetDirectoryName" (substitute #\/ #\\ path)))

;;; A packaged application carries its packages next to the executable, which is
;;; where `dotcl pack --bundle' puts them.
(deftest nlk-bundled-root-sits-beside-the-executable
  (let ((root (substitute #\/ #\\ (nuget:bundled-root)))
        (exe (substitute #\/ #\\ (dotnet:static "System.Environment" "ProcessPath"))))
    (list (equal (nlk-parent root)
                 (dotnet:static "System.IO.Path" "GetDirectoryName" exe))
          (equal "nuget" (dotnet:static "System.IO.Path" "GetFileName" root))))
  (t t))

(deftest nlk-cache-root-sits-beside-the-fasl-cache
  (let ((nuget-root (nuget:cache-root))
        (fasl-root (funcall (find-symbol "%FASL-CACHE-ROOT" "DOTCL"))))
    (list (equal (nlk-parent nuget-root) (nlk-parent fasl-root))
          (and (search "dotcl-nuget" (substitute #\/ #\\ nuget-root)) t)))
  (t t))

;;; The project's record is named for dotcl, not packages.lock.json: a dotcl
;;; project is often also a .NET project, whose own restore owns that name.
(deftest nlk-lock-file-is-in-the-project-directory
  (let ((nuget:*project-directory* "/some/project/"))
    (substitute #\/ #\\ (nuget::%lock-path)))
  "/some/project/dotcl-nuget.lock.json")

;;; --- reading a lock file ---------------------------------------------------

(deftest nlk-read-lock
  (with-nlk-sandbox ()
    (nlk-lock '(("Newtonsoft.Json" "[13.*, )" "13.0.4"))
              '(("Some.Dependency" "2.0.0")))
    (multiple-value-list (nuget::%read-lock (nuget::%lock-path) (nuget::%current-tfm))))
  ((("Newtonsoft.Json" . "[13.*, )"))
   (("Newtonsoft.Json" . "13.0.4") ("Some.Dependency" . "2.0.0"))
   t))

(deftest nlk-read-lock-without-a-file
  (with-nlk-sandbox ()
    (multiple-value-list (nuget::%read-lock (nuget::%lock-path) (nuget::%current-tfm))))
  (nil nil nil))

;;; --- declared requests -----------------------------------------------------

;;; A floating declaration with nothing recorded stops, and says how to go on.
;;; It stops before anything else: the sandbox is not offline here, so reaching
;;; for NuGet would have been possible, and the message is not the offline one.
(deftest nlk-declared-floating-without-a-lock-is-refused
  (with-nlk-sandbox (:offline nil)
    (let ((msg (nlk-message
                (lambda () (nuget::%ensure (list (nlk-req "Newtonsoft.Json" "13.*"))
                                           :mode :declared)))))
      (list (and (search "Newtonsoft.Json" msg) t)
            (and (search "(nuget:restore)" msg) t)
            (and (search "DOTCL_NUGET_OFFLINE" msg) t))))
  (t t nil))

;;; No version at all is the most floating of all.
(deftest nlk-declared-without-a-version-is-refused
  (with-nlk-sandbox (:offline nil)
    (and (search "(nuget:restore)"
                 (nlk-message
                  (lambda () (nuget::%ensure (list (nlk-req "Plain.Package" (nuget::%spec nil nil)))
                                             :mode :declared))))
         t))
  t)

;;; An exact version is allowed to fetch -- except when told not to.
(deftest nlk-declared-exact-offline-is-refused
  (with-nlk-sandbox (:offline t)
    (and (search "DOTCL_NUGET_OFFLINE"
                 (nlk-message
                  (lambda () (nuget::%ensure (list (nlk-req "Newtonsoft.Json" "13.0.3"))
                                             :mode :declared))))
         t))
  t)

;;; What the lock records is the answer, with no NuGet involved: offline, and
;;; the layout already in the cache, it resolves -- floating spec included.
(deftest nlk-declared-floating-follows-the-lock
  (with-nlk-sandbox (:offline t)
    (nlk-lock '(("Newtonsoft.Json" "[13.*, )" "13.0.1")) '(("Dep.A" "1.0.0")))
    (let* ((pins '(("Dep.A" . "1.0.0") ("Newtonsoft.Json" . "13.0.1")))
           (dir (nlk-fake-layout pins)))
      (list (equal dir (nuget::%ensure (list (nlk-req "Newtonsoft.Json" "13.*"))
                                       :mode :declared))
            (nuget::state-pins (nuget::%state (nuget::%current-rid) (nuget::%current-tfm))))))
  (t (("Dep.A" . "1.0.0") ("Newtonsoft.Json" . "13.0.1"))))

;;; The record has to match the declaration as written. A lock that recorded
;;; another version does not answer this one, and an offline image cannot ask.
(deftest nlk-declared-other-version-than-recorded
  (with-nlk-sandbox (:offline t)
    (nlk-lock '(("Newtonsoft.Json" "[13.0.1, )" "13.0.1")) '())
    (and (search "DOTCL_NUGET_OFFLINE"
                 (nlk-message
                  (lambda () (nuget::%ensure (list (nlk-req "Newtonsoft.Json" "13.0.3"))
                                             :mode :declared))))
         t))
  t)

;;; Adding an exact package next to a recorded floating one would re-resolve the
;;; floating one too (NuGet re-evaluates a project whose references changed), so
;;; that is NUGET:RESTORE's call, not a load's.
(deftest nlk-declared-new-package-beside-a-floating-record
  (with-nlk-sandbox (:offline nil)
    (nlk-lock '(("Newtonsoft.Json" "[13.*, )" "13.0.1")) '())
    (let ((msg (nlk-message
                (lambda () (nuget::%ensure (list (nlk-req "Newtonsoft.Json" "13.*")
                                                 (nlk-req "Other.Package" "1.0.0"))
                                           :mode :declared)))))
      (list (and (search "records floating" msg) t)
            (and (search "(nuget:restore)" msg) t))))
  (t t))

;;; One image loads one version of a package.
(deftest nlk-same-package-two-specs
  (with-nlk-sandbox ()
    (nuget::%merge-requests (nuget::%state "r" "t") (list (nlk-req "P" "1.0.0")))
    (and (search "earlier in this image"
                 (nlk-message (lambda ()
                                (nuget::%merge-requests (nuget::%state "r" "t")
                                                        (list (nlk-req "P" "2.0.0"))))))
         t))
  t)

;;; A refused declaration waits for NUGET:RESTORE. A later REQUIRE by hand does
;;; not resolve it behind the lock's back, and the image remembers it so that
;;; RESTORE knows what to record.
(deftest nlk-refused-declaration-stays-pending
  (with-nlk-sandbox (:offline t)
    (nlk-message (lambda () (nuget::%ensure (list (nlk-req "Newtonsoft.Json" "13.*"))
                                            :mode :declared)))
    (let ((r (first (nuget::state-requests
                     (nuget::%state (nuget::%current-rid) (nuget::%current-tfm))))))
      (list (nuget::req-id r) (nuget::req-resolved r))))
  ("Newtonsoft.Json" nil))

;;; --- versions stay where they were registered ------------------------------

(deftest nlk-merge-pins-extends
  (nuget::%merge-pins '(("A" . "1.0.0")) '(("B" . "2.0.0") ("a" . "1.0.0")))
  (("A" . "1.0.0") ("B" . "2.0.0")))

;;; A registered assembly may be loaded already, and .NET will not load a second
;;; one of the same name: a resolution that moves it is refused.
(deftest nlk-merge-pins-refuses-a-move
  (and (search "registered at 1.0.0"
               (nlk-message (lambda () (nuget::%merge-pins '(("A" . "1.0.0"))
                                                           '(("A" . "1.0.1"))))))
       t)
  t)

;;; --- bundled layouts -------------------------------------------------------

(defun nlk-bundle (requests pins)
  (let ((dir (nuget::%combine nuget::*bundle-directory* "some-layout")))
    (nlk-file dir "Bundled.dll" "not an assembly")
    (nuget::%write-marker dir (nuget::%current-rid) (nuget::%current-tfm) pins
                          (mapcar (lambda (r) (nlk-req (first r) (second r))) requests))
    dir))

;;; What the application shipped with answers any request it lists, floating or
;;; not, by hand or declared -- offline, so nothing else could have.
(deftest nlk-bundled-layout-answers-listed-requests
  (with-nlk-sandbox (:offline t)
    (let ((dir (nlk-bundle '(("A" "13.*") ("B" "1.0.0")) '(("A" . "13.0.4") ("B" . "1.0.0")))))
      (list (equal dir (nuget::%ensure (list (nlk-req "A" "13.*" :imperative))))
            ;; the set grows and the same bundle still answers it
            (equal dir (nuget::%ensure (list (nlk-req "B" "1.0.0")) :mode :declared)))))
  (t t))

;;; A request the bundle does not list is not answered by it.
(deftest nlk-bundled-layout-ignores-unlisted
  (with-nlk-sandbox (:offline t)
    (nlk-bundle '(("A" "13.*")) '(("A" . "13.0.4")))
    (and (search "DOTCL_NUGET_OFFLINE"
                 (nlk-message (lambda () (nuget::%ensure (list (nlk-req "A" "13.0.4" :imperative))))))
         t))
  t)

;;; --- what `dotcl pack' carries beside the executable ------------------------

(defun nlk-stage-states (states rid)
  "STATES is a list of (rid out-dir requests pins). Stage them; return the count
and the bundle directory."
  (let ((nuget::*states* (make-hash-table :test #'equal))
        (bundle (nlk-new-dir "staged")))
    (loop for (r dir reqs pins) in states
          do (setf (gethash (list r "net10.0") nuget::*states*)
                   (nuget::make-state :requests reqs :pins pins :out-dir dir)))
    (list (nuget:stage-bundle bundle rid) bundle)))

(defun nlk-layout-dir (tag)
  (let ((d (nlk-new-dir tag)))
    (nlk-file d "Some.Package.dll" tag)
    (nlk-file d "runtimes/win-arm64/native/libfoo.dll" "native too")
    d))

;;; The copy keeps the layout's directory name and its subdirectories (native
;;; assets live under runtimes/), and its marker lists the image's requests as
;;; written -- which is what a shipped program's request is matched against.
(deftest nlk-stage-bundle-copies-and-marks
  (let ((src (nlk-layout-dir "win")))
    (destructuring-bind (n bundle)
        (nlk-stage-states (list (list "win-arm64" src
                                      (list (nlk-req "Some.Package" "13.*"))
                                      '(("Some.Package" . "13.0.4"))))
                          nil)
      (let* ((dir (nuget::%combine bundle "nuget"
                                   (dotnet:static "System.IO.Path" "GetFileName" src)))
             (m (nuget::%read-marker dir)))
        (list n
              (nlk-exists (nuget::%combine dir "Some.Package.dll"))
              (nlk-exists (nuget::%combine dir "runtimes/win-arm64/native/libfoo.dll"))
              (getf m :rid)
              (getf m :requests)))))
  (1 t t "win-arm64" (("Some.Package" "13.*"))))

;;; Each RID package carries its own platform's layout and nothing else.
(deftest nlk-stage-bundle-takes-one-rid
  (let ((states (list (list "win-arm64" (nlk-layout-dir "w") '() '(("P" . "1.0.0")))
                      (list "linux-x64" (nlk-layout-dir "l") '() '(("P" . "1.0.0"))))))
    (list (first (nlk-stage-states states "win-arm64"))
          (first (nlk-stage-states states "linux-x64"))
          (first (nlk-stage-states states nil))))
  (1 1 2))
