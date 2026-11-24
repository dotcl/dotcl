;;; A bad argument to an arithmetic, character or sequence function is a
;;; TYPE-ERROR whose text names the function and the offending value.

(declaim (notinline %haw-id))
(defun %haw-id (x) x)

(defun %haw-message (thunk)
  (handler-case (progn (funcall thunk) nil)
    (type-error (e) (princ-to-string e))))

(defmacro %haw-names (form fn &optional (value ""))
  `(let ((m (%haw-message (lambda () ,form))))
     (and m (search ,fn m) (search ,value m) t)))

(deftest hostile-arg-wording.plus (%haw-names (+ 1 (%haw-id "a")) "+:" "\"a\"") t)
(deftest hostile-arg-wording.minus (%haw-names (- (%haw-id 'sym) 1) "-:" "SYM") t)
(deftest hostile-arg-wording.times (%haw-names (* 2 (%haw-id "x")) "*:" "\"x\"") t)
(deftest hostile-arg-wording.less (%haw-names (< 1 (%haw-id "x")) "<:" "\"x\"") t)
(deftest hostile-arg-wording.sqrt (%haw-names (sqrt (%haw-id 'a)) "SQRT:" "A") t)
(deftest hostile-arg-wording.log (%haw-names (log (%haw-id 'a)) "LOG:" "A") t)
(deftest hostile-arg-wording.atan (%haw-names (atan 1 (%haw-id "b")) "ATAN:" "\"b\"") t)
(deftest hostile-arg-wording.floor (%haw-names (floor 1 (%haw-id 'a)) "FLOOR:" "A") t)
(deftest hostile-arg-wording.char-second-arg
  (%haw-names (char< #\a (%haw-id 1)) "CHAR<:" "1") t)
(deftest hostile-arg-wording.subseq-end-before-start
  (%haw-names (subseq (list 1 2) 1 (%haw-id 0)) "SUBSEQ:" "before the start") t)
(deftest hostile-arg-wording.make-package-names-value
  (%haw-names (make-package (%haw-id 5)) "string designator" "5") t)
(deftest hostile-arg-wording.map-bad-result-type
  (let ((m (%haw-message (lambda () (map (%haw-id 'no-such-seq-type) #'list '(1))))))
    (and m (search "NO-SUCH-SEQ-TYPE" m) (not (search "(OR LIST VECTOR)" m)) t))
  t)

(defun %haw-expected (thunk)
  (handler-case (progn (funcall thunk) nil)
    (type-error (e) (type-error-expected-type e))))

(deftest hostile-arg-wording.compare-expects-real
  (list (%haw-expected (lambda () (< 1 (%haw-id "x"))))
        (%haw-expected (lambda () (max 1 (%haw-id 'a))))
        (%haw-expected (lambda () (floor 1 (%haw-id 'a))))
        (%haw-expected (lambda () (atan 1 (%haw-id "b")))))
  (real real real real))

(deftest hostile-arg-wording.make-package-expects-string-designator
  (equal (%haw-expected (lambda () (make-package (%haw-id 5))))
         '(or string symbol character))
  t)

(deftest hostile-arg-wording.encode-universal-time-ranges
  (list (%haw-expected (lambda () (encode-universal-time 0 0 0 32 1 2000)))
        (%haw-expected (lambda () (encode-universal-time 0 0 0 30 2 2000)))
        (%haw-expected (lambda () (encode-universal-time 0 60 0 1 1 2000)))
        (%haw-expected (lambda () (encode-universal-time 0 0 0 1 13 2000))))
  ((integer 1 31) (integer 1 29) (integer 0 59) (integer 1 12)))

(deftest hostile-arg-wording.sleep-names-function
  (%haw-names (sleep (%haw-id 'a)) "SLEEP:" "A") t)

(deftest hostile-arg-wording.open-names-value
  (%haw-names (open (%haw-id 5)) "pathname designator: 5") t)

(deftest hostile-arg-wording.sort-key-not-function
  (list (%haw-names (sort (list 3 1 2) #'< :key (%haw-id 5)) "SORT:" "5")
        (%haw-names (stable-sort (list 3 1 2) #'< :key (%haw-id 5)) "STABLE-SORT:" "5")
        (sort (list 3 1 2) #'< :key nil))
  (t t (1 2 3)))

;;; A :test / :key / predicate argument that is no function designator is a
;;; TYPE-ERROR that names the function it was given to.
(deftest hostile-arg-wording.function-argument-names-caller
  (let ((l (list 1 2 3)) (b (%haw-id 5)))
    (flet ((names (thunk fn)
             (let ((m (%haw-message thunk)))
               (and m (eql 0 (search (concatenate 'string fn ":") m)) (search "5" m) t))))
      (list (names (lambda () (remove 1 l :test b)) "REMOVE")
            (names (lambda () (find 1 l :key b)) "FIND")
            (names (lambda () (position 1 l :test b)) "POSITION")
            (names (lambda () (count 1 l :key b)) "COUNT")
            (names (lambda () (member 1 l :test b)) "MEMBER")
            (names (lambda () (assoc 1 '((1 . 2)) :key b)) "ASSOC")
            (names (lambda () (reduce b l)) "REDUCE")
            (names (lambda () (find-if b l)) "FIND-IF")
            (names (lambda () (remove-if-not b l)) "REMOVE-IF-NOT")
            (names (lambda () (some b l)) "SOME")
            (names (lambda () (substitute 0 1 l :test b)) "SUBSTITUTE")
            (names (lambda () (search l l :test b)) "SEARCH")
            (names (lambda () (mismatch l l :key b)) "MISMATCH")
            (names (lambda () (mapcar b l)) "MAPCAR")
            (names (lambda () (apply b nil)) "APPLY")
            (names (lambda () (funcall b)) "FUNCALL"))))
  (t t t t t t t t t t t t t t t t))

(deftest hostile-arg-wording.list-length-dotted
  (list (%haw-names (list-length (%haw-id '(1 . 2))) "LIST-LENGTH:" "2")
        (%haw-names (list-length (%haw-id '(1 2 . 3))) "LIST-LENGTH:" "3")
        (%haw-names (list-length (%haw-id 5)) "LIST-LENGTH:" "5")
        (list-length (list 1 2 3)))
  (t t t 3))

(deftest hostile-arg-wording.file-position-non-stream
  (%haw-names (file-position (%haw-id 5)) "FILE-POSITION:" "5") t)

(deftest hostile-arg-wording.set-operations-name-caller
  (let ((l (list 1 2)) (b (%haw-id 5)))
    (flet ((names (thunk fn)
             (let ((m (%haw-message thunk)))
               (and m (eql 0 (search (concatenate 'string fn ":") m)) (search "5" m) t))))
      (list (names (lambda () (union l l :test b)) "UNION")
            (names (lambda () (intersection l l :key b)) "INTERSECTION")
            (names (lambda () (set-difference l l :test b)) "SET-DIFFERENCE")
            (names (lambda () (subsetp l l :test b)) "SUBSETP")
            (names (lambda () (map 'list b l)) "MAP")
            (names (lambda () (mapc b l)) "MAPC")
            (names (lambda () (maplist b l)) "MAPLIST")
            (names (lambda () (mapcan b l)) "MAPCAN"))))
  (t t t t t t t t))
