;;;; Differential test: %mini-eval (tree-walk interpreter) vs eval (compiler).
;;;; The compiler is the oracle — for each form, the interpreted result must
;;;; EQUAL the compiled result (or both must signal an error). This validates
;;;; the emit-free evaluator that will back eval on AOT/IL2CPP/ns2.0 targets.
;;;; Run: dotnet run ... -- --asm compiler/cil-out.sil test/interp/diff.lisp

(defpackage :dotcl-interp-diff (:use :cl))
(in-package :dotcl-interp-diff)

(defvar *pass* 0)
(defvar *fail* 0)
(defvar *fails* '())

(defun mini (form)
  (dotcl.cil-compiler::%mini-eval form nil))

;; Compare interpret vs compile for one form. Both-error counts as agreement.
(defun check1 (form)
  (let ((c-val nil) (c-err nil) (i-val nil) (i-err nil))
    (handler-case (setq c-val (multiple-value-list (eval form)))
      (error (e) (setq c-err e)))
    (handler-case (setq i-val (multiple-value-list (mini form)))
      (error (e) (setq i-err e)))
    (cond
      ((and c-err i-err) (incf *pass*))            ; both error: agree
      ((or c-err i-err)
       (incf *fail*)
       (push (list form :compile (if c-err :error c-val)
                   :interp (if i-err :error i-val)) *fails*))
      ((equalp c-val i-val) (incf *pass*))      ; equalp descends arrays/strings
      (t (incf *fail*)
         (push (list form :compile c-val :interp i-val) *fails*)))))

(defmacro check (&rest forms)
  `(progn ,@(mapcar (lambda (f) `(check1 ',f)) forms)))

;; --- Battery: each form exercised through both evaluators ---
(check
  ;; literals / arithmetic
  42 "hi" #\a (+ 1 2 3) (* 2 (+ 3 4)) (- 10 3 2) (/ 12 4)
  ;; let / let* / setq
  (let ((x 5) (y 7)) (+ x y))
  (let* ((x 5) (y (* x 2))) (+ x y))
  (let ((x 1)) (setq x (+ x 10)) x)
  ;; if / when / unless / cond / and / or (macros + if)
  (if t 1 2) (if nil 1 2) (when (> 3 2) 'yes) (unless (> 3 2) 'no)
  (cond ((= 1 2) 'a) ((= 1 1) 'b) (t 'c))
  (and 1 2 3) (or nil nil 5) (and 1 nil 3)
  ;; lambda / funcall / apply / flet / labels
  (funcall (lambda (a b) (* a b)) 6 7)
  (flet ((sq (x) (* x x))) (sq 9))
  (labels ((fact (n) (if (< n 2) 1 (* n (fact (1- n)))))) (fact 5))
  (mapcar (lambda (x) (* x x)) '(1 2 3 4))
  ;; iteration macros
  (let ((acc 0)) (dolist (x '(1 2 3 4) acc) (incf acc x)))
  (let ((acc 0)) (dotimes (i 5 acc) (incf acc i)))
  ;; block / return-from / tagbody / go
  (block b (return-from b 99) 7)
  (let ((s 0)) (dotimes (i 3) (block inner (when (= i 1) (return-from inner)) (incf s))) s)
  (let ((i 0) (s 0)) (tagbody top (when (< i 5) (incf s i) (incf i) (go top))) s)
  ;; multiple values
  (multiple-value-bind (q r) (floor 17 5) (list q r))
  (multiple-value-list (values 1 2 3))
  (multiple-value-call #'list (values 1 2) (values 3 4))
  (multiple-value-prog1 (values 'a 'b) 'ignored)
  ;; catch / throw / unwind-protect
  (catch 'tag (throw 'tag 123) 7)
  (let ((s 0)) (catch 'k (unwind-protect (throw 'k 1) (setq s 42))) s)
  (unwind-protect 5 nil)
  ;; progv (dynamic)
  (progv '(*xx*) '(10) (symbol-value '*xx*))
  ;; destructuring / nested
  (destructuring-bind (a (b c) &rest d) '(1 (2 3) 4 5) (list a b c d))
  ;; closures capturing
  (let ((c (let ((n 0)) (lambda () (incf n))))) (list (funcall c) (funcall c) (funcall c)))
  ;; conditions
  (handler-case (error "boom") (error (e) (princ-to-string e)))
  (handler-case (/ 1 0) (division-by-zero () 'caught))
  ;; condition edge cases
  (handler-case 42 (error () 'never))                    ; no condition: body value
  (handler-case (values 1 2 3) (error () 'never))        ; MV through handler-case
  (handler-case (error "x") (type-error () 'wrong) (error () 'right)) ; clause order
  (handler-case (signal 'error) (error () 'sig))
  (ignore-errors (+ 1 2))
  ;; KNOWN LIMITATION (not asserted): (ignore-errors (error "x")) returns the
  ;; condition as a 2nd value when compiled; the interpreter currently returns
  ;; only the primary NIL — multiple values are dropped when a handler does a
  ;; non-local exit through the native Signal frame. Primary value is correct.
  ;; handler-bind that declines -> propagates to outer handler-case
  (handler-case
      (handler-bind ((warning (lambda (c) (declare (ignore c)) nil)))
        (error "deep"))
    (error () 'outer-caught))
  ;; handler-bind handler does non-local exit
  (block done
    (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from done 'bound-exit))))
      (error "hb")))
  ;; nested handler-case
  (handler-case (handler-case (error "inner") (type-error () 'te))
    (error () 'outer))
  ;; more macros: case / typecase / when-multi / push-pop / do / loop
  (case 2 (1 'a) (2 'b) (t 'c))
  (typecase "hi" (integer 'i) (string 's) (t 'o))
  (let ((l '())) (push 1 l) (push 2 l) (pop l) l)
  (do ((i 0 (1+ i)) (s 0 (+ s i))) ((= i 5) s))
  (loop for i from 1 to 5 collect (* i i))
  (loop for x in '(a b c) for i from 0 collect (cons i x))
  (reduce #'+ '(1 2 3 4 5))
  (remove-if #'evenp '(1 2 3 4 5 6))
  ;; --- sequences ---
  (mapcar #'+ '(1 2 3) '(10 20 30))
  (mapcan (lambda (x) (list x (* x x))) '(1 2 3))
  (reduce #'cons '(1 2 3) :from-end t :initial-value nil)
  (remove 3 '(1 2 3 4 3 2 1))
  (find-if #'plusp '(-1 -2 3 -4))
  (position #\b "abcabc")
  (count #\a "banana")
  (sort (list 3 1 2 5 4) #'<)
  (every #'numberp '(1 2 3))
  (some #'stringp '(1 "x" 3))
  (subseq '(a b c d e) 1 3)
  (nthcdr 2 '(a b c d))
  (last '(1 2 3 4))
  (butlast '(1 2 3 4))
  (mapcar #'car '((1 . a) (2 . b)))
  (assoc 'b '((a . 1) (b . 2)))
  (member 3 '(1 2 3 4))
  (reverse (list 1 2 3))
  (append '(1 2) '(3 4) '(5))
  (length (vector 1 2 3 4))
  ;; --- strings ---
  (concatenate 'string "ab" "cd")
  (string-upcase "hello")
  (subseq "common-lisp" 0 6)
  (char "lisp" 2)
  (search "lo" "hello world")
  (string= "abc" "abc")
  (format nil "~a-~s-~d" 'x "y" 42)
  (format nil "~{~a~^,~}" '(1 2 3))
  (format nil "~5,'0d" 42)
  ;; --- numbers ---
  (floor 17 5)
  (truncate -7 2)
  (mod -1 3)
  (gcd 12 18)
  (lcm 4 6)
  (expt 2 10)
  (isqrt 50)
  (max 3 1 4 1 5)
  (min 3 1 4 1 5)
  (abs -7)
  (logand 12 10)
  (logior 12 10)
  (ash 1 4)
  (/ 6 4)
  (rationalize 0.5)
  (float 1/4)
  (round 5/2)
  ;; --- types / predicates ---
  (typep 5 'integer)
  (typep "x" 'string)
  (typep 'a 'symbol)
  (coerce '(1 2 3) 'vector)
  (coerce 65 'character)
  (etypecase 3 (string 's) (number 'n))
  (numberp 3.0)
  (eql 'a 'a)
  (equal '(1 2) '(1 2))
  (equalp "ABC" "abc")
  ;; --- arrays / vectors ---
  (let ((a (make-array 3 :initial-element 0))) (setf (aref a 1) 9) (aref a 1))
  (aref #(10 20 30) 2)
  (let ((v (make-array 0 :adjustable t :fill-pointer 0)))
    (vector-push-extend 1 v) (vector-push-extend 2 v) (coerce v 'list))
  ;; --- multiple values / destructuring ---
  (multiple-value-bind (a b c) (values 1 2 3) (+ a b c))
  (nth-value 1 (floor 17 5))
  (destructuring-bind (a &optional (b 9) &rest r) '(1) (list a b r))
  (destructuring-bind (&key x y) '(:x 1 :y 2) (list x y))
  ;; --- hash tables ---
  (let ((h (make-hash-table))) (setf (gethash 'k h) 7) (gethash 'k h))
  (let ((h (make-hash-table :test 'equal))) (setf (gethash "s" h) 1) (gethash "s" h))
  ;; --- closures / higher order ---
  (funcall (let ((n 10)) (lambda (x) (+ x n))) 5)
  (mapcar (let ((c 0)) (lambda (x) (incf c x))) '(1 2 3))
  (apply #'+ 1 2 '(3 4))
  ;; --- loop clauses ---
  (loop for i from 1 to 10 sum i)
  (loop for i in '(1 2 3 4) count (evenp i))
  (loop for i in '(3 1 4 1 5) maximize i)
  (loop for i in '(3 1 4 1 5) minimize i)
  (loop for i from 1 to 5 when (oddp i) collect i)
  (loop for i from 1 to 5 unless (oddp i) collect i)
  (loop for i from 0 below 5 collect i)
  (loop for i downfrom 5 to 1 collect i)
  (loop for x on '(1 2 3) collect x)
  (loop for x across "abc" collect x)
  (loop for i from 1 while (< i 4) collect i)
  (loop for i from 1 until (> i 3) collect i)
  (loop for i from 1 to 3 append (list i i))
  (loop for i from 1 to 3 nconc (list i (* i 10)))
  (loop for (k v) on '(:a 1 :b 2) by #'cddr collect (cons k v))
  (loop with s = 0 for i from 1 to 4 do (incf s i) finally (return s))
  (loop for i from 1 to 5 collect i into xs finally (return (reverse xs)))
  (loop repeat 3 collect 'x)
  (loop for i = 1 then (* i 2) repeat 5 collect i)
  ;; --- format directives ---
  (format nil "~b ~o ~x" 10 10 255)
  (format nil "~r" 7)
  (format nil "~:d" 1234567)
  (format nil "~3,2f" 3.14159)
  (format nil "~a~%~a" 'a 'b)
  (format nil "~c" #\A)
  (format nil "~:(~a~)" "hello world")
  (format nil "~@(~a~)" "hello world")
  (format nil "~d item~:p" 3)
  (format nil "~d item~:p" 1)
  (format nil "~{~a=~a~^ ~}" '(a 1 b 2))
  (format nil "~[zero~;one~;two~]" 1)
  (format nil "~v,'0d" 4 7)
  ;; --- chars ---
  (char-code #\A)
  (code-char 97)
  (char-upcase #\x)
  (char-downcase #\Y)
  (alpha-char-p #\5)
  (digit-char-p #\7)
  (char< #\a #\b)
  (alphanumericp #\_)
  ;; --- symbols / packages ---
  (symbol-name 'foo)
  (keywordp :foo)
  ;; intern / gensym omitted here: stateful, and the harness evaluates each form
  ;; twice (compiled then interpreted), so the second call legitimately sees
  ;; different state — not a differential signal.
  (find-symbol "CAR" :cl)
  (symbol-package 'car)
  ;; --- strings ---
  (string-trim " " "  hi  ")
  (string-left-trim "ab" "aabbcc")
  (parse-integer "  42  " :junk-allowed t)
  (write-to-string '(1 2 3))
  (read-from-string "(+ 1 2)")
  (string-equal "Foo" "foo")
  (string< "abc" "abd")
  (reverse "lisp")
  (substitute #\x #\a "banana")
  (remove-duplicates '(1 2 1 3 2 4))
  ;; --- list ops ---
  (mapc #'identity '(1 2 3))
  (maplist #'car '(1 2 3))
  (copy-list '(1 2 3))
  (list* 1 2 '(3 4))
  (getf '(:a 1 :b 2) :b)
  (union '(1 2 3) '(2 3 4))
  (intersection '(1 2 3) '(2 3 4))
  (set-difference '(1 2 3 4) '(2 4))
  (nreverse (list 1 2 3))
  (mapcar #'1+ (loop for i to 3 collect i))
  ;; --- numbers (more) ---
  (sqrt 16)
  (isqrt 17)
  (ceiling 7 2)
  (rem 7 3)
  (signum -5)
  (numerator 3/4)
  (denominator 3/4)
  (rationalp 1/2)
  (evenp 0)
  (zerop 0.0)
  (1+ 41)
  (* 1/2 4)
  (expt 2 -1)
  (log 8 2)
  ;; --- control flow ---
  (prog1 'a 'b 'c)
  (prog2 'a 'b 'c)
  (cond (nil 1) (t 2))
  (let ((x 5)) (cond ((evenp x) 'even) ((oddp x) 'odd)))
  (not nil)
  (let ((acc 0)) (dotimes (i 4) (when (= i 2) (return)) (incf acc)) acc)
  (values-list '(1 2 3))
  (multiple-value-list (truncate 17 5))
  ;; --- restarts / handler-case extras ---
  (restart-case (invoke-restart 'r 7) (r (x) (* x 10)) (s () 'nope))
  (restart-case (+ 1 2) (r () 'restart))
  (handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'use 5))))
    (restart-case (error "x") (use (v) (* v v))))
  (restart-case (if (find-restart 'r) :found :missing) (r () 'x))
  (block done
    (handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'cont))))
      (restart-bind ((cont (lambda () (return-from done :continued))))
        (error "x"))))
  (handler-case (values 1 2) (:no-error (a b) (+ a b)))
  (handler-case (error "e") (error () :err) (:no-error (v) v))
  ;; (ignore-errors's 2nd value is a fresh condition object each eval, so it is
  ;; not equalp across the two runs — verified separately that interp returns
  ;; (NIL <condition>) like the compiler.
  (with-simple-restart (skip "skip it") (invoke-restart 'skip))
  ;; --- lambda-list edges: &aux, supplied-p, ((:kw var) ...) ---
  (funcall (lambda (a &aux (b (* a 2)) (c (+ a b))) (list a b c)) 5)
  (funcall (lambda (x &aux y) (list x y)) 3)
  (funcall (lambda (a &optional (b 9 b-p)) (list a b b-p)) 1)
  (funcall (lambda (a &optional (b 9 b-p)) (list a b b-p)) 1 2)
  (funcall (lambda (&key (x 0 x-p)) (list x x-p)))
  (funcall (lambda (&key (x 0 x-p)) (list x x-p)) :x 7)
  (funcall (lambda (&key ((:foo bar) 10)) bar) :foo 42)
  (funcall (lambda (&key ((:foo bar) 10 p)) (list bar p)))
  (funcall (lambda (a &optional b &rest r &key k) (list a b r k))
           1 2 :k 3)
  (funcall (lambda (n &aux (sq (* n n))) sq) 6)
  ;; --- pathnames ---
  (pathname-name (pathname "foo.lisp"))
  (pathname-type (pathname "foo.lisp"))
  (file-namestring (pathname "dir/foo.txt"))
  (namestring (make-pathname :name "bar" :type "dat"))
  (pathname-type (merge-pathnames "a.lisp" "b.txt"))
  (pathnamep (pathname "x"))
  (enough-namestring "/a/b/c.txt" "/a/")
  ;; --- string streams ---
  (with-output-to-string (s) (princ "hello" s) (princ 42 s))
  (with-input-from-string (s "12 34 56") (list (read s) (read s) (read s)))
  (let ((s (make-string-output-stream)))
    (write-string "abc" s) (write-char #\! s) (get-output-stream-string s))
  (with-output-to-string (s) (format s "~A=~D" :x 7))
  (with-input-from-string (s "hello world")
    (list (read-char s) (read-char s) (peek-char nil s)))
  (with-output-to-string (s) (dotimes (i 3) (write i :stream s)))
  ;; --- packages ---
  (package-name (find-package :cl))
  (find-symbol "CONS" :cl)
  (nth-value 1 (find-symbol "LIST" :cl))
  (let ((p (or (find-package "DIFF-TMP-PKG")
               (make-package "DIFF-TMP-PKG" :use '(:cl)))))
    (list (packagep p) (package-name p)))
  (symbol-name (intern "ALPHA" :keyword))
  (eq (find-package :keyword) (symbol-package :foo))
  (mapcar #'symbol-name
          (sort (loop for s being the external-symbols of :keyword
                      when (string= (symbol-name s) "TEST-NONEXISTENT-XYZ")
                      collect s)
                #'string<))
  ;; --- rare def-forms: deftype / define-symbol-macro / define-modify-macro /
  ;;     defstruct setf accessor.
  ;; (define-setf-expander + same-unit (setf (dmid ...)) is intentionally NOT here:
  ;;  the compiler expands the setf at COMPILE time, before the expander is
  ;;  registered at load time, so it errors — a legitimate compile/runtime ordering
  ;;  difference, not an interpreter bug. The interpreter evaluates sequentially and
  ;;  handles it; verified separately.)
  (progn (deftype dsmall () '(integer 0 9))
         (list (typep 5 'dsmall) (typep 50 'dsmall) (typep -1 'dsmall)))
  (progn (define-symbol-macro *dsm-x* (* 6 7)) *dsm-x*)
  (progn (define-symbol-macro *dsm-y* (list :a :b)) (length *dsm-y*))
  (progn (define-modify-macro dmult (n) *) (let ((x 5)) (dmult x 4) (dmult x 2) x))
  (progn (defstruct dbox v) (let ((b (make-dbox :v 1))) (setf (dbox-v b) 7) (dbox-v b)))
  ;; --- setf places / parallel assignment / mv assignment ---
  (let ((p (list :a 1))) (setf (getf p :b) 2) p)
  (let ((a 1) (b 2)) (psetq a b b a) (list a b))
  (let ((x (list 1 2))) (psetf (car x) (cadr x) (cadr x) (car x)) x)
  (let ((a 1) (b 2) (c 3)) (rotatef a b c) (list a b c))
  (let ((a 1) (b 2)) (list (shiftf a b 9) a b))
  (let (a b) (multiple-value-setq (a b) (floor 17 5)) (list a b))
  (let (a b) (setf (values a b) (floor 17 5)) (list a b))
  (let ((x 0)) (setf (ldb (byte 4 0) x) 5) x)
  (let ((x (list nil))) (push 5 (car x)) (push 6 (car x)) x)
  ;; --- do / do* / control macros ---
  (do ((i 0 (1+ i)) (s 0 (+ s i))) ((= i 5) s))
  (do* ((i 0 (1+ i)) (j (* i 2) (* i 2))) ((= i 3) j))
  (typecase 3.5 (integer :int) (float :float) (t :other))
  (etypecase "x" (integer :int) (string :str))
  ;; --- destructuring edges ---
  (destructuring-bind (&whole w a b) '(1 2) (list w a b))
  (destructuring-bind (a (b c) &rest d) '(1 (2 3) 4 5) (list a b c d))
  ;; --- type system: parametric/composed deftype, typep specifiers, subtypep ---
  (progn (deftype rng (lo hi) `(integer ,lo ,hi))
         (list (typep 5 '(rng 0 9)) (typep 15 '(rng 0 9))))
  (progn (deftype non-neg () '(integer 0 *))
         (deftype small-nn () '(and non-neg (integer * 9)))
         (list (typep 5 'small-nn) (typep -1 'small-nn) (typep 20 'small-nn)))
  (list (typep 3 '(or integer string)) (typep "x" '(or integer string))
        (typep 'a '(or integer string)))
  (typep 5 '(and integer (satisfies oddp)))
  (list (typep :b '(member :a :b :c)) (typep :z '(member :a :b :c)))
  (typep '(1 . 2) '(cons integer integer))
  (typep (make-array 3) '(array t (3)))
  (list (typep 5 '(not string)) (typep "x" '(not string)))
  (multiple-value-list (subtypep 'integer 'number))
  (multiple-value-list (subtypep '(integer 0 9) 'integer))
  (coerce '(1 2 3) 'vector)
  (coerce 1 'double-float)
  (the integer (+ 2 3))
  (typep #c(1 2) '(complex integer))
  ;; --- advanced loop clauses (expand to tagbody/go — stress interp) ---
  (loop named outer for i from 0 do (when (= i 3) (return-from outer i)))
  (loop for x in '(1 3 5 8) thereis (and (evenp x) x))
  (loop for x in '(2 4 6) always (evenp x))
  (loop for x in '(1 3 5) never (evenp x))
  ;; hash iteration (sum is order-independent across the two evaluations)
  (let ((h (make-hash-table))) (setf (gethash 'a h) 1 (gethash 'b h) 2 (gethash 'c h) 3)
    (loop for k being the hash-keys of h using (hash-value v) sum v))
  (let ((h (make-hash-table))) (setf (gethash 'a h) 10 (gethash 'b h) 20)
    (loop for v being the hash-values of h count (> v 5)))
  (loop for i from 1 to 5 collect i into xs sum i into total
        finally (return (list (length xs) total)))
  (loop for i from 1 to 6 when (evenp i) collect i else collect (- i))
  (loop for i from 0 to 3 and j downfrom 10 collect (cons i j))
  (loop for i from 1 to 3 nconc (loop for j from 1 to i collect j))
  ;; --- prog / prog* / prog1 / prog2 ---
  (prog ((a 1) (b 2)) (when (< a b) (go skip)) (return :no) skip (return (+ a b)))
  (prog* ((a 5) (b (* a 2))) (return (list a b)))
  (let ((x 0)) (list (prog1 (incf x) (incf x) (incf x)) x))
  (prog2 (+ 1 1) (+ 2 2) (+ 3 3))
  ;; --- iterator macros + advanced format directives ---
  (let ((h (make-hash-table))) (setf (gethash 'a h) 1 (gethash 'b h) 2 (gethash 'c h) 4)
    (with-hash-table-iterator (it h)
      (let ((s 0)) (loop (multiple-value-bind (more k v) (it)
                           (declare (ignore k))
                           (if more (incf s v) (return s)))))))
  (format nil "~{~A~^, ~}" '(1 2 3))
  (format nil "~[zero~;one~;two~]" 1)
  (format nil "~?" "~A-~A" '(1 2))
  (format nil "~A ~2*~A" 1 2 3 4)
  (format nil "~D cat~:P" 3)
  (format nil "~D cat~:P" 1)
  (format nil "~:@(~A~)" "hello")
  (format nil "~3,'0D" 7)
  (format nil "~{~A=~A~^ ~}" '(:a 1 :b 2))
  (let ((v (make-array 3))) (map-into v #'+ '(1 2 3) '(10 20 30)) (coerce v 'list))
  (reduce #'+ '((1) (2) (3)) :key #'car)
  (merge 'list (list 1 3 5) (list 2 4 6) #'<)
  (with-output-to-string (s) (write-string "hi" s) (terpri s) (princ 42 s))
  ;; --- special-form interaction edges (return-from implicit fn block, shadowing, nlx) ---
  (labels ((f (n) (if (< n 0) (return-from f :neg) (* n 2)))) (list (f 3) (f -1)))
  (flet ((g (n) (when (> n 0) (return-from g :pos)) :nonpos)) (list (g 5) (g -1)))
  (labels ((rec (n acc) (if (zerop n) (return-from rec acc) (rec (1- n) (+ acc n))))) (rec 5 0))
  (macrolet ((m (n) (if (zerop n) 0 `(+ ,n (m ,(1- n)))))) (m 5))
  ;; NOTE: two shadowing cases are intentionally excluded — the interpreter is
  ;; ANSI-correct but the COMPILER (the oracle) is wrong, so they cannot use
  ;; compiler-as-oracle. Tracked as a separate compiler bug:
  ;;   (flet ((lst ...)) (macrolet ((lst ...)) (lst 1 2)))   ; macrolet must shadow flet
  ;;   (let ((x 1)) (symbol-macrolet ((x 99)) (+ x x)))      ; symbol-macrolet must shadow let
  (flet ((+ (a b) (list :plus a b))) (+ 1 2))
  (labels ((car (x) (list :mycar x))) (car '(1 2)))
  (multiple-value-call #'list (values 1 2) (values 3 4) 5)
  (multiple-value-call #'+ (floor 17 5) (floor 9 2))
  (let ((log '()))
    (block b (unwind-protect (unwind-protect (return-from b :inner)
                               (push :inner-cleanup log))
               (push :outer-cleanup log)))
    (reverse log))
  (let ((log '())) (catch 'tag (unwind-protect (throw 'tag :x) (push :cleanup log))) log)
  (let ((fns '())) (dolist (i '(1 2 3)) (let ((j i)) (push (lambda () j) fns)))
    (mapcar #'funcall (reverse fns)))
  (progn (defvar *dv* 0) (let ((*dv* 42)) (funcall (lambda () *dv*))))
  (let ((s 0)) (tagbody (dolist (x '(1 2 3)) (incf s x) (when (= x 2) (go done))) done) s)
  )

(format t "~%=== interp diff: ~D PASSED  ~D FAILED  (of ~D) ===~%"
        *pass* *fail* (+ *pass* *fail*))
(dolist (f (reverse *fails*))
  (format t "~%MISMATCH: ~S~%   compile=> ~S~%   interp => ~S~%"
          (first f) (third f) (fifth f)))
(dotcl:quit (if (zerop *fail*) 0 1))
