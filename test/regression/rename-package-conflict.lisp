;;; RENAME-PACKAGE signals a PACKAGE-ERROR, and renames nothing, when the new name
;;; or a new nickname is already the name or a nickname of another package.

(deftest rename-package-conflict.name
  (progn
    (make-package "RPC-A") (make-package "RPC-B")
    (list (handler-case (progn (rename-package "RPC-A" "RPC-B") :renamed)
            (package-error () :package-error))
          (package-name (find-package "RPC-A"))
          (package-name (find-package "RPC-B"))))
  (:package-error "RPC-A" "RPC-B"))

(deftest rename-package-conflict.nickname
  (progn
    (make-package "RPC-C") (make-package "RPC-D" :nicknames '("RPC-DN"))
    (list (handler-case (progn (rename-package "RPC-C" "RPC-C2" '("RPC-DN")) :renamed)
            (package-error () :package-error))
          (package-name (find-package "RPC-C"))
          (find-package "RPC-C2")))
  (:package-error "RPC-C" nil))

(deftest rename-package-conflict.same-package-ok
  (progn
    (make-package "RPC-E" :nicknames '("RPC-EN"))
    (list (package-name (rename-package "RPC-E" "RPC-E" '("RPC-EN")))
          (package-name (rename-package "RPC-E" "RPC-F"))
          (find-package "RPC-E")))
  ("RPC-E" "RPC-F" nil))
