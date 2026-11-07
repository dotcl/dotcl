;;; After (require "asdf"), the bundled contrib directories are on asdf:*central-registry*, each once, and a
;;; bundled system is found by name. They are pushed onto the symbol's value directly, not by evaluating a
;;; PUSHNEW form for each directory.

(require "asdf")

(deftest asdf-contrib-registry.bundled-system-found
  (not (null (asdf:find-system "dotcl-gray" nil)))
  t)

(deftest asdf-contrib-registry.no-duplicates
  (let ((r asdf:*central-registry*))
    (= (length r) (length (remove-duplicates r :test #'equal))))
  t)

(deftest asdf-contrib-registry.contrib-dirs-present
  (not (null (member-if (lambda (p) (and (pathnamep p) (search "dotcl-gray" (namestring p))))
                        asdf:*central-registry*)))
  t)
