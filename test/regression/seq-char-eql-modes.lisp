;;; FIND / POSITION / COUNT / REMOVE / ASSOC with a character (or fixnum) item compare the elements by value:
;;; same answers as the general EQL for characters of every code, other element types, :key, :test,
;;; :test-not, :from-end, :start / :end.

(defvar *sce-l* (list #\a #\b (code-char 0) (code-char 255) (code-char 256) (code-char 65535) 5 :k "s" 1.5 #\a))
(defvar *sce-v* (coerce *sce-l* 'vector))

(deftest seq-char-eql-modes.chars
  (list (position #\a *sce-l*) (position #\a *sce-l* :from-end t) (position (code-char 0) *sce-l*)
        (position (code-char 255) *sce-l*) (position (code-char 256) *sce-l*)
        (position (code-char 65535) *sce-l*) (position #\z *sce-l*) (position 97 *sce-l*)
        (position #\a *sce-v*) (position #\a *sce-v* :start 1) (position #\a *sce-l* :start 1 :end 10)
        (find #\b *sce-l*) (find (code-char 65535) *sce-l*) (find #\z *sce-l*)
        (count #\a *sce-l*) (count #\a *sce-v*) (count #\a *sce-l* :start 1)
        (remove #\a *sce-l*) (length (remove #\a *sce-v*)) (remove #\a *sce-l* :count 1)
        (position #\A *sce-l* :test #'char-equal) (position #\a *sce-l* :test-not #'eql)
        (position #\a (mapcar (lambda (c) (list c)) (list #\x #\a)) :key #'car)
        (count #\a *sce-l* :test (lambda (a b) (and (characterp b) (char= a b)))))
  (0 10 2 3 4 5 nil nil 0 10 nil #\b #\U+FFFF nil 2 2 1
   (#\b #\Nul #\ÿ #\Ā #\U+FFFF 5 :k "s" 1.5) 9 (#\b #\Nul #\ÿ #\Ā #\U+FFFF 5 :k "s" 1.5 #\a)
   0 1 1 2))

(deftest seq-char-eql-modes.assoc
  (let ((al (list (cons #\a 1) nil (cons (code-char 65535) 2) (cons 5 3) (cons 1.5 4) (cons :k 5) (cons #\a 6))))
    (list (assoc #\a al) (assoc (code-char 65535) al) (assoc 5 al) (assoc 1.5 al) (assoc :k al)
          (assoc #\z al) (assoc 6 al) (assoc 97 al) (assoc #\A al :test #'char-equal)
          (assoc 5 al :key #'identity) (assoc nil al)
          (handler-case (assoc #\a '(1 2)) (type-error () :type-error))
          (handler-case (assoc 5 '((1 . 2) . 3)) (type-error () :type-error))))
  ((#\a . 1) (#\U+FFFF . 2) (5 . 3) (1.5 . 4) (:k . 5) nil nil nil (#\a . 1) (5 . 3) nil :type-error :type-error))
