;;; SEARCH reaches its keyword shapes without building an argument array.
;;;
;;; SEARCH was the only sequence builtin with no direct entry at all -- not even
;;; the two-argument one, whose implementation already existed but was never
;;; installed -- so every call allocated a LispObject[] for the arguments. What
;;; these pin is the ANSI behaviour across the shapes the direct entries now
;;; answer: no keywords, and one to three keyword pairs, plus the args-array
;;; fallback beyond that.

(deftest search-direct.no-keywords
  (list (search "bc" "abcd")
        (search "" "abc")
        (search "zz" "abc")
        (search '(2 3) '(1 2 3 4)))
  (1 0 nil 1))

(deftest search-direct.one-keyword-pair
  (list (search "b" "abcb" :from-end t)
        (search "B" "abcb" :test #'char-equal)
        (search "b" "abcb" :start2 2))
  (3 1 3))

(deftest search-direct.two-keyword-pairs
  (list (search "b" "abcb" :start2 1 :end2 2)
        (search "bc" "xxabcd" :start2 0 :end2 6)
        (search "ab" "abab" :start2 1 :end2 4))
  (1 3 2))

(deftest search-direct.three-keyword-pairs
  (list (search "B" "abcb" :start2 0 :end2 4 :test #'char-equal)
        (search "b" "abcb" :start2 0 :end2 4 :from-end t)
        (search '(1) '(0 1 0 1) :start2 0 :end2 4 :from-end t))
  (1 3 3))

;;; Past three pairs the args-array path still runs, and must agree.
(deftest search-direct.four-keyword-pairs
  (search "B" "abcb" :start2 0 :end2 4 :test #'char-equal :from-end t)
  3)

;;; :start1 / :end1 select inside the pattern.
(deftest search-direct.pattern-bounds
  (list (search "xbcx" "abcd" :start1 1 :end1 3)
        (search "xbcx" "abcd" :start1 1 :end1 3 :start2 0))
  (1 1))

;;; :key applies to BOTH sequences (CLHS 17.2), so the pattern is keyed too:
;;; (1) keys to (1), (2 4 6) keys to (0 0 0), and there is no match. Checked
;;; against SBCL -- the intuitive answer here is the wrong one.
(deftest search-direct.key
  (search (quote (1)) (quote (2 4 6)) :key (lambda (x) (mod x 2)))
  nil)

;;; :test-not is the complement of :test.
(deftest search-direct.test-not
  (search "a" "aab" :test-not #'char=)
  2)

;;; Duplicate keywords are first-wins, the same on the direct and array paths.
(deftest search-direct.duplicate-keyword-first-wins
  (list (search "b" "abcb" :start2 3 :start2 0)
        (search "b" "abcb" :start2 3 :start2 0 :end2 4 :end2 1))
  (3 3))

;;; An unknown keyword is a PROGRAM-ERROR unless :allow-other-keys says otherwise.
(deftest search-direct.unknown-keyword-signals
  (handler-case (search "b" "abcb" :bogus 1)
    (program-error () :program-error)
    (error (e) (type-of e)))
  :program-error)

(deftest search-direct.allow-other-keys
  (search "b" "abcb" :bogus 1 :allow-other-keys t)
  1)

;;; Out-of-range bounds are still checked.
(deftest search-direct.bad-bounds-signals
  (handler-case (search "b" "abc" :start2 9)
    (error () :error))
  :error)

;;; --- the direct entries exist so the call does not build an argument array ---

(defun %sde-bytes () (nth 4 (dotcl:gc-stats)))

(defmacro %sde-per-call (name &body body)
  `(progn
     (defun ,name (n)
       (declare (fixnum n))
       (let ((r nil))
         (do ((i 0 (1+ i))) ((= i n) r)
           (declare (fixnum i))
           (setq r (progn ,@body)))))
     (,name 2000)
     (let ((best nil))
       (dotimes (r 5 best)
         (let ((before (%sde-bytes)))
           (,name 20000)
           (let ((used (- (%sde-bytes) before)))
             (when (or (null best) (< used best)) (setq best used))))))))

(defvar *sde-pat* "foo=")
(defvar *sde-text*
  (with-output-to-string (s)
    (dotimes (i 200) (format s "line ~a foo=~a~%" i i))))
(defvar *sde-end* (length *sde-text*))

(deftest-compiled-only search-direct.allocates-nothing
  (list (= 0 (%sde-per-call %sde-plain (search *sde-pat* *sde-text*)))
        (= 0 (%sde-per-call %sde-two (search *sde-pat* *sde-text*
                                             :start2 0 :end2 *sde-end*)))
        (= 0 (%sde-per-call %sde-three (search *sde-pat* *sde-text*
                                               :start2 0 :end2 *sde-end*
                                               :test #'char=))))
  (t t t))
