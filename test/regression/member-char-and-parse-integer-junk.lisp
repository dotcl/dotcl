;;; (MEMBER x l :test #'CHAR-EQUAL / #'CHAR= / #'EQL) and (PARSE-INTEGER s :start :end :junk-allowed)
;;; have direct entries; the answers and the errors are those of the general functions.

(defun %mcp-ci (x l) (member x l :test #'char-equal))
(defun %mcp-cs (x l) (member x l :test #'char=))
(defun %mcp-eql (x l) (member x l :test #'eql))

(deftest mcp.char-equal
  (list (%mcp-ci #\t '(#\a #\T #\b)) (%mcp-ci #\T '(#\a #\t)) (%mcp-ci #\z '(#\a #\b)) (%mcp-ci #\a nil)
        (%mcp-ci #\a '(#\a)) (eq (cdr (%mcp-ci #\a '(#\x #\A #\y))) (cddr '(#\x #\A #\y))))
  ((#\T #\b) (#\t) nil nil (#\a) nil))

(deftest mcp.char=
  (list (%mcp-cs #\t '(#\a #\T #\t #\b)) (%mcp-cs #\T '(#\a #\t)) (%mcp-cs #\a '(#\a #\b)))
  ((#\t #\b) nil (#\a #\b)))

(deftest mcp.eql
  (list (%mcp-eql 3 '(1 2 3 4)) (%mcp-eql 'x '(a b)) (%mcp-eql 1.5 '(1.5)) (%mcp-eql "a" '("a")))
  ((3 4) nil (1.5) nil))

(deftest mcp.errors-are-those-of-the-test-function
  (list (handler-case (%mcp-ci 5 '(#\a)) (type-error () :type-error))
        (handler-case (%mcp-ci #\a '(1 #\a)) (type-error () :type-error))
        (handler-case (%mcp-cs #\a '(#\b . 5)) (type-error () :type-error))
        (%mcp-ci #\a '(#\a . 5)))
  (:type-error :type-error :type-error (#\a . 5)))

(defun %mcp-pi1 (s) (multiple-value-list (parse-integer s :start 1 :end 5 :junk-allowed t)))
(defun %mcp-pi2 (s) (multiple-value-list (parse-integer s :junk-allowed t :end 5 :start 1)))
(defun %mcp-pi3 (s) (multiple-value-list (parse-integer s :end nil :start 1 :junk-allowed nil)))
(defun %mcp-pi4 (s) (parse-integer s :start 1 :end 4 :junk-allowed t))

(deftest mcp.parse-integer-junk
  (list (%mcp-pi1 "x123x45") (%mcp-pi2 "x123x45") (%mcp-pi1 "x  7  ") (%mcp-pi1 "xabcde") (%mcp-pi1 "x-12ab")
        (%mcp-pi3 "x42") (%mcp-pi4 "x12ab") (%mcp-pi4 "x   "))
  ((123 4) (123 4) (7 4) (nil 1) (-12 4) (42 3) 12 nil))

(deftest mcp.parse-integer-errors
  (list (handler-case (parse-integer "x12" :start 1 :end 9 :junk-allowed t) (error (e) (type-of e)))
        (handler-case (parse-integer "x12" :start "a" :end 3 :junk-allowed t) (type-error () :type-error))
        (handler-case (parse-integer "x1z" :start 1 :end 3 :junk-allowed nil) (parse-error () :parse-error))
        (handler-case (%mcp-pi3 "xab") (parse-error () :parse-error)))
  (type-error :type-error :parse-error :parse-error))
