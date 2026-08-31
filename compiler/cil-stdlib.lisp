;;; cil-stdlib.lisp — Standard library functions implemented in Lisp
;;; These are compiled alongside user code to provide higher-order
;;; sequence functions, set operations, and other utilities.
;;;
;;; NOTE: No (in-package ...) here — this file is compiled as user code,
;;; not loaded into the compiler's own package.

;;; ============================================================
;;; Built-in function wrappers (for #'eql, #'eq, etc.)
;;; ============================================================

;; These are compiled as real functions so that (function eql) works.
;; The body calls the built-in compiler primitive which emits Runtime.Eql etc.

(defun eq (a b) (eq a b))
(defun eql (a b) (eql a b))
(defun equal (a b) (equal a b))
(defun not (x) (not x))
(defun identity (x) x)
(defun car (x) (car x))
(defun cdr (x) (cdr x))
(defun cons (a b) (cons a b))
(defun atom (x) (atom x))
(defun null (x) (null x))
(defun numberp (x) (numberp x))
(defun integerp (x) (integerp x))
(defun symbolp (x) (symbolp x))
(defun stringp (x) (stringp x))
(defun characterp (x) (characterp x))
(defun functionp (x) (functionp x))
(defun consp (x) (consp x))
(defun listp (x) (listp x))
(defun zerop (x) (zerop x))
(defun plusp (x) (plusp x))
(defun minusp (x) (minusp x))
(defun evenp (x) (evenp x))
(defun oddp (x) (oddp x))
(defun typep (x type &optional env)
  ;; The environment parameter is currently ignored.
  ;; CLHS specifies that env is used for type expansion (deftype).
  ;; It will be implemented when deftype environment support is added.
  (declare (ignore env))
  (typep x type))
(defun subtypep (type1 type2 &optional env)
  (declare (ignore env))
  (subtypep type1 type2))
(defun aref (array &rest indices)
  ;; #'aref via funcall/apply. Direct (aref a i j ...) is handled rank-aware by the
  ;; compiler; here the rank-0..3 cases route back to that fast path, and rank >=4
  ;; goes through row-major-aref + array-row-major-index (both arbitrary-rank C#
  ;; primitives), so any rank works.
  (cond
    ((null indices) (aref array))
    ((null (cdr indices)) (aref array (car indices)))
    ((null (cddr indices)) (aref array (car indices) (cadr indices)))
    ((null (cdddr indices)) (aref array (car indices) (cadr indices) (caddr indices)))
    (t (row-major-aref array (apply #'array-row-major-index array indices)))))
(defun length (x) (length x))
(defun keywordp (x) (keywordp x))
(defun char (s i) (char s i))
; CHAR= is variadic; registered in Startup.cs as N-arg
(defun maphash (fn ht) (maphash fn ht))
(defun symbol-name (x) (symbol-name x))
(defun symbol-package (x) (symbol-package x))
(defun find-package (x) (find-package x))
(defun package-name (x) (package-name x))
(defun package-nicknames (x) (package-nicknames x))
(defun package-use-list (x) (package-use-list x))
(defun package-used-by-list (x) (package-used-by-list x))
(defun package-shadowing-symbols (x) (package-shadowing-symbols x))
(defun list-all-packages () (list-all-packages))
(defun delete-package (x) (delete-package x))
(defun make-package (name &key nicknames use) (make-package name :nicknames nicknames :use use))
(defun rename-package (pkg new-name &optional new-nicknames) (rename-package pkg new-name new-nicknames))
(defun find-symbol (name &optional (pkg *package*)) (find-symbol name pkg))
(defun intern (name &optional (pkg *package*)) (intern name pkg))
(defun unintern (sym &optional (pkg *package*)) (unintern sym pkg))
(defun export (symbols &optional (pkg *package*))
  (if (listp symbols)
      (dolist (s symbols t) (%package-export pkg s))
      (%package-export pkg symbols)))
(defun import (symbols &optional (pkg *package*))
  (if (listp symbols)
      (dolist (s symbols t) (%package-import pkg s))
      (%package-import pkg symbols)))
(defun shadow (names &optional (pkg *package*))
  (if (listp names)
      (dolist (n names t) (%package-shadow pkg n))
      (%package-shadow pkg names)))
(defun shadowing-import (symbols &optional (pkg *package*))
  (if (listp symbols)
      (dolist (s symbols t) (%shadowing-import s pkg))
      (%shadowing-import symbols pkg)))
(defun unexport (symbols &optional (pkg *package*))
  (if (listp symbols)
      (dolist (s symbols t) (%unexport s pkg))
      (%unexport symbols pkg)))
(defun use-package (pkg-to-use &optional (pkg *package*))
  (if (listp pkg-to-use)
      (dolist (p pkg-to-use t) (%package-use pkg p))
      (%package-use pkg pkg-to-use)))
(defun unuse-package (pkg-to-remove &optional (pkg *package*))
  (if (listp pkg-to-remove)
      (dolist (p pkg-to-remove t) (%unuse-package p pkg))
      (%unuse-package pkg-to-remove pkg)))
; reverse and string are implemented in C# (Runtime.Sequences.cs / Runtime.cs)
(defun apply (fn &rest args)
  (if (cdr args)
      (apply fn (apply #'list* args))
      (apply fn (car args))))
(defun first (x) (car x))
(defun second (x) (cadr x))
(defun third (x) (caddr x))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun cadddr (x) (car (cdr (cdr (cdr x)))))
(defun fourth (x) (cadddr x))
(defun fifth (x) (car (cdr (cdr (cdr (cdr x))))))
(defun sixth (x) (car (cdr (cdr (cdr (cdr (cdr x)))))))
(defun seventh (x) (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))))
(defun eighth (x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))
(defun ninth (x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))))
(defun tenth (x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))))
(defun rest (x) (cdr x))
(defun endp (x)
  (unless (listp x)
    (error 'type-error :datum x :expected-type 'list))
  (null x))

(defun %check-index (n fn)
  "Check n is a non-negative integer, signal type-error if not."
  (unless (and (integerp n) (>= n 0))
    (error 'type-error :datum n :expected-type '(integer 0 *))))

(defun last (list &optional (n 1))
  (%check-index n "LAST")
  ;; A dotted list is fine here -- (last '(a b . c)) is (b . c) -- but a
  ;; non-list is not: the walk below simply hands the argument back, so
  ;; (last 7) answered 7 instead of reporting it (BUTLAST already checks).
  (unless (listp list)
    (error 'type-error :datum list :expected-type 'list))
  ;; Return the last n conses of list (two-pointer approach)
  (let ((lead list))
    (dotimes (i n)
      (unless (consp lead) (return-from last list))
      (setq lead (cdr lead)))
    (do ((lag list (cdr lag)))
        ((not (consp lead)) lag)
      (setq lead (cdr lead)))))

(defun butlast (list &optional (n 1))
  (unless (listp list)
    (error 'type-error :datum list :expected-type 'list))
  (%check-index n "BUTLAST")
  (let ((lead list))
    (dotimes (i n)
      (unless (consp lead) (return-from butlast nil))
      (setq lead (cdr lead)))
    (let ((result nil))
      (do ((lag list (cdr lag)))
          ((not (consp lead)) (nreverse result))
        (push (car lag) result)
        (setq lead (cdr lead))))))

(defun nbutlast (list &optional (n 1))
  (unless (listp list)
    (error 'type-error :datum list :expected-type 'list))
  (%check-index n "NBUTLAST")
  (let ((lead list))
    (dotimes (i n)
      (unless (consp lead) (return-from nbutlast nil))
      (setq lead (cdr lead)))
    (unless (consp lead) (return-from nbutlast nil))
    (do ((lag list (cdr lag)))
        ((not (consp (cdr lead)))
         (rplacd lag nil)
         list)
      (setq lead (cdr lead)))))

(defun list-length (list)
  ;; Signal type-error for non-list
  (unless (listp list)
    (error 'type-error :datum list :expected-type 'list))
  ;; Tortoise-and-hare: returns NIL for circular lists per CL spec
  ;; For dotted lists, CDR-on-atom naturally signals type-error
  (do ((n 0 (+ n 2))
       (fast list (cddr fast))
       (slow list (cdr slow)))
      (nil)
    (when (endp fast) (return n))
    (when (endp (cdr fast)) (return (+ n 1)))
    (when (and (> n 0) (eq fast slow)) (return nil))))
(defun nconc (&rest lists)
  (let ((result nil))
    (dolist (l lists result)
      (if (null result)
          (setq result l)
          (let ((last (last result)))
            (rplacd last l)
            (setq result result))))))
(defun copy-tree (tree)
  (if (consp tree)
      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))
      tree))
(defun string (x) (string x))
(defun gethash (key ht &optional default) (gethash key ht default))
(defun intern (name &optional (pkg *package*))
  (intern name pkg))
(defun gensym (&optional (prefix "G")) (gensym prefix))

;; Arithmetic wrappers (body uses 2-arg inline ops, not self-recursive)
(defun 1+ (x) (+ x 1))
(defun 1- (x) (- x 1))

(defun + (&rest args)
  (let ((result 0))
    (dolist (x args result) (setq result (+ result x)))))
;; CLHS: (- number &rest more) — zero arguments is a program-error. The unary case
;; is a sign flip, not "subtract from 0": (- 0.0) is -0.0, and compile-sub uses
;; Runtime.Negate for the same reason. The (- x) / (- result x) below lower to the
;; inline two-argument op, so this is not self-recursive.
(defun - (&rest args)
  (if (null args)
      (error 'program-error)
      (if (null (cdr args))
          (- (car args))
          (let ((result (car args)))
            (dolist (x (cdr args) result) (setq result (- result x)))))))
(defun * (&rest args)
  (let ((result 1))
    (dolist (x args result) (setq result (* result x)))))
;; Comparison ops are variadic per CLHS (number &rest more-numbers). The body
;; uses the 2-arg inline op (compiled to a direct Runtime call, NOT a self-call),
;; so #'= etc. accept any arity and match (= 1 2 3) direct-call semantics. Direct
;; multi-arg calls are still inlined by the compiler; these defuns back funcall/apply.
(defun = (number &rest more)
  (dolist (x more t) (unless (= number x) (return nil))))
(defun < (number &rest more)
  (let ((prev number))
    (dolist (x more t) (if (< prev x) (setq prev x) (return nil)))))
(defun > (number &rest more)
  (let ((prev number))
    (dolist (x more t) (if (> prev x) (setq prev x) (return nil)))))
(defun <= (number &rest more)
  (let ((prev number))
    (dolist (x more t) (if (<= prev x) (setq prev x) (return nil)))))
(defun >= (number &rest more)
  (let ((prev number))
    (dolist (x more t) (if (>= prev x) (setq prev x) (return nil)))))

;; Attach 2-arg direct delegates to the comparison wrapper function objects
;; just defined (#'< handed to SORT etc. — the funcall path). Compiled call
;; sites already inline via compile-nary-comparison; this covers the
;; function-object path with the same C# comparators the 2-arg wrapper
;; bodies compile to (identical T/NIL results and type errors).
(%attach-numeric-compare-fast-paths)
;; Same for the arithmetic wrappers: #'+ handed to REDUCE / MAPCAR built an
;; args array and a &rest list per call.
(%attach-arith-fast-paths)

;;; ============================================================
;;; Sequence functions (list-specialized)
;;; ============================================================

;;; find, find-if, find-if-not are implemented in C# (Runtime.Find/FindIf) for performance

;;; remove, remove-if, remove-if-not, delete, delete-if, delete-if-not are implemented in C# (Runtime.RemoveFull/RemoveIf)


;;; substitute, substitute-if, substitute-if-not, nsubstitute, nsubstitute-if, nsubstitute-if-not
;;; are implemented in C# (Runtime.SubstituteFull/SubstituteIf/NsubstituteFull/NsubstituteIf)

;;; count, count-if, count-if-not are implemented in C# (Runtime.Count/CountIf) for performance

;;; position, position-if, position-if-not are implemented in C# (Runtime.Position/PositionIf) for performance

;;; reduce is implemented in C# (Runtime.Reduce) for performance

;;; every, some, notevery, notany are implemented in C# (Runtime.Every/Some)

(defun %map-multi (tails)
  ;; Helper: collect one step of args from cursor list.
  ;; Returns the args list (reversed), or nil if any cursor is exhausted.
  ;; Advances cursors in place.
  (let ((args nil))
    (do ((cs tails (cdr cs)))
        ((null cs) args)
      (let ((tail (car cs)))
        (when (null tail) (return nil))
        (push (car tail) args)
        (rplaca cs (cdr tail))))))

(defun %maplist-multi (tails)
  ;; Like %map-multi but collects tails (not cars) as args.
  (let ((args nil))
    (do ((cs tails (cdr cs)))
        ((null cs) args)
      (let ((tail (car cs)))
        (when (null tail) (return nil))
        (push tail args)
        (rplaca cs (cdr tail))))))

(defun mapc (function list &rest more-lists)
  (if (null more-lists)
      (dolist (x list list)
        (funcall function x))
      (let ((tails (cons list more-lists)))
        (do ((args (%map-multi tails) (%map-multi tails)))
            ((null args) list)
          (apply function (nreverse args))))))

(defun mapcan (function list &rest more-lists)
  (if (null more-lists)
      ;; Single list: keep a pointer to the tail instead of NCONCing the whole
      ;; result again per element, and call FUNCTION directly instead of
      ;; building an argument list for APPLY. NCONC's own rules are kept: a NIL
      ;; piece contributes nothing, and a non-list piece is only allowed last.
      (let ((head nil) (tail nil))
        (dolist (x list head)
          (let ((piece (funcall function x)))
            (cond ((null piece))
                  ((null head)
                   (setq head piece)
                   (setq tail (and (consp piece) (last piece))))
                  ((null tail)
                   (error 'type-error :datum head :expected-type 'list))
                  (t
                   (setf (cdr tail) piece)
                   (setq tail (and (consp piece) (last piece))))))))
      (let ((tails (cons list more-lists))
            (result nil))
        (do ((args (%map-multi tails) (%map-multi tails)))
            ((null args) result)
          (setq result (nconc result (apply function (nreverse args))))))))

(defun maplist (function list &rest more-lists)
  (if (null more-lists)
      (let ((result nil))
        (do ((tail list (cdr tail)))
            ((null tail) (nreverse result))
          (push (funcall function tail) result)))
      (let ((tails (cons list more-lists))
            (result nil))
        (do ((args (%maplist-multi tails) (%maplist-multi tails)))
            ((null args) (nreverse result))
          (push (apply function (nreverse args)) result)))))

(defun mapl (function list &rest more-lists)
  (if (null more-lists)
      (do ((tail list (cdr tail)))
          ((null tail) list)
        (funcall function tail))
      (let ((tails (cons list more-lists)))
        (do ((args (%maplist-multi tails) (%maplist-multi tails)))
            ((null args) list)
          (apply function (nreverse args))))))

(defun mapcon (function list &rest more-lists)
  (if (null more-lists)
      (let ((result nil))
        (do ((tail list (cdr tail)))
            ((null tail) result)
          (setq result (nconc result (funcall function tail)))))
      (let ((tails (cons list more-lists))
            (result nil))
        (do ((args (%maplist-multi tails) (%maplist-multi tails)))
            ((null args) result)
          (setq result (nconc result (apply function (nreverse args))))))))

;;; ============================================================
;;; Type predicates
;;; ============================================================

(defun realp (x) (and (numberp x) (not (complexp x))))

;;; ============================================================
;;; List operations: LDIFF, TAILP, COPY-ALIST, REVAPPEND, NRECONC
;;; ============================================================

(defun tailp (object list)
  ;; Returns T if object is EQL to any tail (sublist) of list, including NIL
  (do ((tail list (cdr tail)))
      ((atom tail) (eql tail object))
    (when (eq tail object) (return t))))

(defun ldiff (list object)
  ;; Returns a fresh list of the elements of list before object (using eql)
  ;; If object not found, return a copy of the whole list (incl. dotted tail)
  (unless (listp list)
    (error 'type-error :datum list :expected-type 'list))
  (do ((tail list (cdr tail))
       (result nil))
      ((atom tail)
       ;; Hit atom tail: include it if not eql to object
       (if (eql tail object)
           (nreverse result)
           (nconc (nreverse result) tail)))
    (when (eq tail object) (return (nreverse result)))
    (push (car tail) result)))

(defun copy-alist (alist)
  (let ((result nil))
    (dolist (pair alist (nreverse result))
      (if (consp pair)
          (push (cons (car pair) (cdr pair)) result)
          (push pair result)))))

(defun revappend (list tail)
  (do ((rest list (cdr rest))
       (result tail))
      ((null rest) result)
    (setq result (cons (car rest) result))))

(defun nreconc (list tail)
  (do ((rest list)
       (result tail))
      ((null rest) result)
    (let ((next (cdr rest)))
      (rplacd rest result)
      (setq result rest)
      (setq rest next))))

;;; ============================================================
;;; Property list operations
;;; ============================================================

(defun %member-proper (item indicator-list)
  "Like member but signals type-error for dotted indicator-list."
  (do ((inds indicator-list (cdr inds)))
      ((null inds) nil)
    (unless (consp inds)
      (error 'type-error :datum inds :expected-type 'list))
    (when (eq item (car inds))
      (return inds))))

(defun get-properties (plist indicator-list)
  ;; Search plist for any indicator in indicator-list
  ;; Returns: indicator, value, tail (or NIL NIL NIL if not found)
  (do ((tail plist (cddr tail)))
      ((null tail) (values nil nil nil))
    (let ((indicator (car tail)))
      (when (%member-proper indicator indicator-list)
        (return (values indicator (cadr tail) tail))))))

;;; ============================================================
;;; Association list operations
;;; ============================================================

;;; member, member-if, member-if-not are implemented in C# (Runtime.Member/MemberIf) for performance

;;; assoc, assoc-if, assoc-if-not, rassoc, rassoc-if, rassoc-if-not
;;; are implemented in C# (Runtime.Assoc/AssocIf/Rassoc/RassocIf) for performance

(defun acons (key datum alist)
  (cons (cons key datum) alist))

(defun pairlis (keys data &optional alist)
  (do ((k keys (cdr k))
       (d data (cdr d))
       (result alist (cons (cons (car k) (car d)) result)))
      ((null k) result)))

;;; ============================================================
;;; Set operations
;;; ============================================================

;;; ADJOIN lives in C# (Runtime.AdjoinFull and its 2/4/6-arg direct entries).
;;; A &key Lisp defun gets no typed direct delegate, so every (adjoin item list
;;; :test ...) — which is what PUSHNEW expands into — went through the variadic
;;; XEP and an args array.

;;; Helper: test if item (already key-applied) is in list using test/test-not/key
;;; Calls (test item (key x)) — item is first arg
(defun %set-member (item list test test-not key)
  (dolist (x list nil)
    (let ((k (funcall key x)))
      (when (if test-not
                (not (funcall test-not item k))
                (funcall test item k))
        (return t)))))

;;; Like %set-member but calls (test (key x) item) — item is second arg
(defun %set-member-rev (item list test test-not key)
  (dolist (x list nil)
    (let ((k (funcall key x)))
      (when (if test-not
                (not (funcall test-not k item))
                (funcall test k item))
        (return t)))))

;;; The set functions all have the same shape: walk LIST1 and ask whether each
;;; element is in LIST2. Asking used to always go through %SET-MEMBER, which
;;; takes five arguments -- and a five-argument call to a *named* function
;;; allocates an array to record its call frame (the frame keeps its first four
;;; arguments in inline slots and has nowhere to put a fifth). That was 64 bytes
;;; per element of LIST1, on functions that otherwise cost only their result:
;;; (subsetp l5 l5), which builds nothing at all, cost 368 bytes.
;;;
;;; With no :TEST, :TEST-NOT or :KEY -- which is how these are nearly always
;;; called -- the question is exactly what MEMBER answers, and MEMBER is a C#
;;; builtin with a two-argument direct entry. The supplied-p flags decide, so
;;; passing :TEST #'EQL explicitly still takes the general path rather than
;;; being second-guessed.

(defun union (list1 list2 &key (test nil test-p) test-not (key nil key-p))
  (let ((result (copy-list list2)))
    (if (or test-p test-not key-p)
        (let ((test (if test-p test #'eql))
              (key (or key #'identity)))
          (dolist (x list1 result)
            (unless (%set-member (funcall key x) list2 test test-not key)
              (push x result))))
        (dolist (x list1 result)
          (unless (member x list2) (push x result))))))

(defun nunion (list1 list2 &key (test nil test-p) test-not (key nil key-p))
  (if (or test-p test-not key-p)
      (union list1 list2 :test (if test-p test #'eql) :test-not test-not :key key)
      (union list1 list2)))

(defun intersection (list1 list2 &key (test nil test-p) test-not (key nil key-p))
  (let ((result nil))
    (if (or test-p test-not key-p)
        (let ((test (if test-p test #'eql))
              (key (or key #'identity)))
          (dolist (x list1 (nreverse result))
            (when (%set-member (funcall key x) list2 test test-not key)
              (push x result))))
        (dolist (x list1 (nreverse result))
          (when (member x list2) (push x result))))))

(defun nintersection (list1 list2 &key (test nil test-p) test-not (key nil key-p))
  (if (or test-p test-not key-p)
      (intersection list1 list2 :test (if test-p test #'eql) :test-not test-not :key key)
      (intersection list1 list2)))

(defun set-difference (list1 list2 &key (test nil test-p) test-not (key nil key-p))
  (let ((result nil))
    (if (or test-p test-not key-p)
        (let ((test (if test-p test #'eql))
              (key (or key #'identity)))
          (dolist (x list1 (nreverse result))
            (unless (%set-member (funcall key x) list2 test test-not key)
              (push x result))))
        (dolist (x list1 (nreverse result))
          (unless (member x list2) (push x result))))))

(defun nset-difference (list1 list2 &key (test nil test-p) test-not (key nil key-p))
  (if (or test-p test-not key-p)
      (set-difference list1 list2 :test (if test-p test #'eql) :test-not test-not :key key)
      (set-difference list1 list2)))

(defun set-exclusive-or (list1 list2 &key (test nil test-p) test-not (key nil key-p))
  ;; Elements in list1 not in list2: test called as (test (key e1) (key e2))
  ;; Elements in list2 not in list1: test also called as (test (key e1) (key e2))
  ;; so for second pass we use %set-member-rev to keep list1-key as first arg
  (let ((result nil))
    (if (or test-p test-not key-p)
        (let ((test (if test-p test #'eql))
              (key (or key #'identity)))
          (dolist (x list1)
            (unless (%set-member (funcall key x) list2 test test-not key)
              (push x result)))
          (dolist (x list2)
            (unless (%set-member-rev (funcall key x) list1 test test-not key)
              (push x result))))
        (progn
          (dolist (x list1)
            (unless (member x list2) (push x result)))
          (dolist (x list2)
            (unless (member x list1) (push x result)))))
    result))

(defun nset-exclusive-or (list1 list2 &key (test nil test-p) test-not (key nil key-p))
  (if (or test-p test-not key-p)
      (set-exclusive-or list1 list2 :test (if test-p test #'eql) :test-not test-not :key key)
      (set-exclusive-or list1 list2)))

(defun subsetp (list1 list2 &key (test nil test-p) test-not (key nil key-p))
  (if (or test-p test-not key-p)
      (let ((test (if test-p test #'eql))
            (key (or key #'identity)))
        (dolist (x list1 t)
          (unless (%set-member (funcall key x) list2 test test-not key)
            (return nil))))
      (dolist (x list1 t)
        (unless (member x list2) (return nil)))))

;;; ============================================================
;;; Tree operations
;;; ============================================================

;;; The recursion used to pass :TEST and :TEST-NOT down at every node, so each
;;; cons in the tree cost a keyword argument list on the way in. They do not
;;; change, so they are carried positionally instead. A LABELS closure would
;;; have read them from the enclosing frame, but that closure is allocated on
;;; every call to TREE-EQUAL, including the ones that answer from an atom
;;; without recursing at all -- a named helper with four parameters costs
;;; nothing (a call frame records its first four arguments in inline slots).
(defun %tree-equal-1 (a b test test-not)
  (if (consp a)
      (and (consp b)
           (%tree-equal-1 (car a) (car b) test test-not)
           (%tree-equal-1 (cdr a) (cdr b) test test-not))
      (and (not (consp b))
           (if test-not
               (not (funcall test-not a b))
               (if (funcall test a b) t nil)))))

(defun tree-equal (tree1 tree2 &key (test #'eql) test-not)
  (%tree-equal-1 tree1 tree2 test test-not))

(defun subst (new old tree &key (test #'eql) test-not (key #'identity))
  (let ((key (or key #'identity)))
    (labels ((s (node)
               (let ((k (funcall key node)))
                 (if (if test-not
                         (not (funcall test-not old k))
                         (funcall test old k))
                     new
                     (if (consp node)
                         (let ((new-car (s (car node)))
                               (new-cdr (s (cdr node))))
                           (if (and (eq new-car (car node))
                                    (eq new-cdr (cdr node)))
                               node
                               (cons new-car new-cdr)))
                         node)))))
      (s tree))))

(defun nsubst (new old tree &key (test #'eql) test-not (key #'identity))
  ;; Genuinely destructive: callers rely on the tree being mutated in place
  ;; and may discard the return value. SBCL's PROPAGATE-LVAR-ANNOTATIONS does
  ;; (nsubst new old (annotation-deps dep)) to redirect dependency lists when
  ;; lvars are substituted; the previous subst-alias left the deps pointing at
  ;; dead lvars (type NIL) and every funarg call-type check warned.
  (let ((key (or key #'identity)))
    (labels ((match-p (node)
               (let ((k (funcall key node)))
                 (if test-not
                     (not (funcall test-not old k))
                     (funcall test old k))))
             (s (node)
               (cond ((match-p node) new)
                     ((consp node)
                      (let ((a (s (car node)))
                            (d (s (cdr node))))
                        (unless (eq a (car node)) (rplaca node a))
                        (unless (eq d (cdr node)) (rplacd node d)))
                      node)
                     (t node))))
      (s tree))))

(defun subst-if (new predicate tree &key (key #'identity))
  (let ((key (or key #'identity)))
    (labels ((s (node)
               (if (funcall predicate (funcall key node))
                   new
                   (if (consp node)
                       (let ((new-car (s (car node)))
                             (new-cdr (s (cdr node))))
                         (if (and (eq new-car (car node))
                                  (eq new-cdr (cdr node)))
                             node
                             (cons new-car new-cdr)))
                       node))))
      (s tree))))

(defun nsubst-if (new predicate tree &key (key #'identity))
  (let ((key (or key #'identity)))
    (labels ((s (node)
               (cond ((funcall predicate (funcall key node)) new)
                     ((consp node)
                      (let ((a (s (car node)))
                            (d (s (cdr node))))
                        (unless (eq a (car node)) (rplaca node a))
                        (unless (eq d (cdr node)) (rplacd node d)))
                      node)
                     (t node))))
      (s tree))))

(defun subst-if-not (new predicate tree &key (key #'identity))
  (let ((key (or key #'identity)))
    (labels ((s (node)
               (if (not (funcall predicate (funcall key node)))
                   new
                   (if (consp node)
                       (let ((new-car (s (car node)))
                             (new-cdr (s (cdr node))))
                         (if (and (eq new-car (car node))
                                  (eq new-cdr (cdr node)))
                             node
                             (cons new-car new-cdr)))
                       node))))
      (s tree))))

(defun nsubst-if-not (new predicate tree &key (key #'identity))
  (let ((key (or key #'identity)))
    (labels ((s (node)
               (cond ((not (funcall predicate (funcall key node))) new)
                     ((consp node)
                      (let ((a (s (car node)))
                            (d (s (cdr node))))
                        (unless (eq a (car node)) (rplaca node a))
                        (unless (eq d (cdr node)) (rplacd node d)))
                      node)
                     (t node))))
      (s tree))))

(defun sublis (alist tree &key (test #'eql) test-not (key #'identity))
  (let ((key (or key #'identity)))
    (labels ((s (node)
               (let* ((k (funcall key node))
                      (pair (if test-not
                                (assoc k alist :test-not test-not)
                                (assoc k alist :test test))))
                 (if pair
                     (cdr pair)
                     (if (consp node)
                         (let ((new-car (s (car node)))
                               (new-cdr (s (cdr node))))
                           (if (and (eq new-car (car node))
                                    (eq new-cdr (cdr node)))
                               node
                               (cons new-car new-cdr)))
                         node)))))
      (s tree))))

(defun nsublis (alist tree &key (test #'eql) test-not (key #'identity))
  (sublis alist tree :test test :test-not test-not :key key))

;;; ============================================================
;;; Miscellaneous
;;; ============================================================

(defun %seq-to-list (s)
  "Convert any sequence to a list."
  (cond
    ((listp s) s)
    (t (let ((r nil))
         (dotimes (i (length s) (nreverse r))
           (push (elt s i) r))))))

(defun %map-rt-category (rt)
  "Return :string, :vector, :bit-vector, :list, :nil, or :unknown for a type specifier."
  (let* ((base (if (consp rt) (car rt) rt))
         (name (if (symbolp base) (symbol-name base) nil)))
    (cond
      ((null rt) :nil)
      ((null name) :unknown)
      ((member name '("STRING" "SIMPLE-STRING" "BASE-STRING" "SIMPLE-BASE-STRING") :test #'string=) :string)
      ;; (vector character) and similar char-vector types → :string
      ((and (string= name "VECTOR") (consp rt) (consp (cdr rt))
            (symbolp (cadr rt))
            (member (symbol-name (cadr rt)) '("CHARACTER" "BASE-CHAR" "STANDARD-CHAR") :test #'string=))
       :string)
      ((member name '("BIT-VECTOR" "SIMPLE-BIT-VECTOR") :test #'string=) :bit-vector)
      ((member name '("VECTOR" "SIMPLE-VECTOR" "ARRAY" "SIMPLE-ARRAY") :test #'string=) :vector)
      ((member name '("LIST" "CONS") :test #'string=) :list)
      ((string= name "NULL") :null)
      (t :unknown))))

(defun map (result-type function &rest sequences)
  (let* ((result-type (if result-type (%typexpand-full result-type) result-type))
         (result nil)
         (cat (%map-rt-category result-type)))
    (cond
      ;; One sequence, which is nearly every call: walk it and call the function
      ;; directly. The parallel path below builds an argument list per element,
      ;; reverses it and APPLYs it.
      ((and sequences (null (cdr sequences)))
       (let ((seq (car sequences)))
         (if (listp seq)
             (dolist (x seq) (push (funcall function x) result))
             (dotimes (i (length seq))
               (push (funcall function (elt seq i)) result)))))
      ;; Two sequences: the shape the rest of the calls have. Walk both with
      ;; plain cursors -- no argument list, no APPLY, and nothing rebuilt per
      ;; element.
      ((and sequences (cdr sequences) (null (cddr sequences)))
       (let ((a (%seq-to-list (first sequences)))
             (b (%seq-to-list (second sequences))))
         (loop
           (when (or (null a) (null b)) (return))
           (push (funcall function (car a) (car b)) result)
           (setq a (cdr a))
           (setq b (cdr b)))))
      (t
       ;; Three or more: step through them in parallel. SEQ-LISTS is built here,
       ;; so the cursors are advanced in place rather than rebuilding the whole
       ;; list of them for every element.
       (let ((seq-lists (mapcar #'%seq-to-list sequences)))
         (block outer
           (loop
             (let ((args nil))
               (do ((c seq-lists (cdr c))) ((null c))
                 (when (null (car c)) (return-from outer)))
               (do ((c seq-lists (cdr c))) ((null c))
                 (push (caar c) args))
               (push (apply function (nreverse args)) result)
               (do ((c seq-lists (cdr c))) ((null c))
                 (setf (car c) (cdar c)))))))))
    (setq result (nreverse result))
    (cond
      ((eq cat :nil) nil)
      ((eq cat :null) (if result (error 'type-error :datum result :expected-type 'null) nil))
      ((eq cat :string) (coerce result 'string))
      ((eq cat :bit-vector)
       (when (and (consp result-type) (consp (cdr result-type)) (integerp (cadr result-type))
                  (/= (length result) (cadr result-type)))
         (error 'type-error :datum result :expected-type result-type))
       (coerce result 'bit-vector))
      ((eq cat :vector)
       ;; Check the compound size constraint. The third element means
       ;; different things per base type (CLHS): (vector elt N) — N is a
       ;; LENGTH; (array elt N) / (simple-array elt N) — N is a RANK, and
       ;; a length constraint is spelled (simple-array elt (N)). SBCL's
       ;; perfectly-hashable maps into (simple-array (unsigned-byte 32) 1)
       ;; — rank 1, any length.
       (when (and (consp result-type) (consp (cdr result-type)) (consp (cddr result-type)))
         (let* ((base-name (symbol-name (car result-type)))
                (arrayp (member base-name '("ARRAY" "SIMPLE-ARRAY") :test #'string=))
                (spec (caddr result-type))
                (required-length
                  (cond ((not arrayp) (and (integerp spec) spec))
                        ;; (array elt (N)) — single-dimension length
                        ((and (consp spec) (integerp (car spec)) (null (cdr spec)))
                         (car spec))
                        (t nil))))
           ;; (array elt RANK) with rank /= 1 is not a sequence type
           (when (and arrayp (integerp spec) (/= spec 1))
             (error 'type-error :datum result :expected-type result-type))
           (when (and required-length (/= (length result) required-length))
             (error 'type-error :datum result :expected-type result-type))))
       (coerce result 'vector))
      ((eq cat :list) result)
      (t
       ;; Unknown/compound type (e.g. (or (vector t 5) (vector t 10)))
       ;; Use subtypep fallback like merge does.
       (multiple-value-bind (sub-list ok1) (subtypep result-type 'list)
         (multiple-value-bind (sub-string ok2) (subtypep result-type 'string)
           (multiple-value-bind (sub-vector ok3) (subtypep result-type 'vector)
             (let ((res (cond
                          ((and ok1 sub-list) result)
                          ((and ok2 sub-string) (coerce result 'string))
                          ((and ok3 sub-vector)
                           ;; For OR types, verify all vector components share the same
                           ;; element type; if they differ the type is ambiguous (MAP.ERROR.10).
                           (when (and (consp result-type) (eq (car result-type) 'or))
                             (let ((first-etype :none))
                               (dolist (sub (cdr result-type))
                                 (let ((etype
                                        (cond
                                          ((and (consp sub) (symbolp (car sub))
                                                (member (symbol-name (car sub))
                                                        '("VECTOR" "SIMPLE-VECTOR") :test #'string=))
                                           (if (and (consp (cdr sub)) (not (eq (cadr sub) '*)))
                                               (cadr sub) 't))
                                          ((and (symbolp sub)
                                                (member (symbol-name sub)
                                                        '("BIT-VECTOR" "SIMPLE-BIT-VECTOR") :test #'string=))
                                           'bit)
                                          ((and (consp sub) (symbolp (car sub))
                                                (member (symbol-name (car sub))
                                                        '("BIT-VECTOR" "SIMPLE-BIT-VECTOR") :test #'string=))
                                           'bit)
                                          (t nil))))
                                   (when etype
                                     (if (eq first-etype :none)
                                         (setq first-etype etype)
                                       (unless (equal (upgraded-array-element-type etype)
                                                      (upgraded-array-element-type first-etype))
                                         (error 'type-error :datum result-type
                                                :expected-type 'sequence))))))))
                           (coerce result 'vector))
                          (t (error 'type-error :datum result-type
                                    :expected-type '(or list vector))))))
               (unless (typep res result-type)
                 (error 'type-error :datum res :expected-type result-type))
               res))))))))

(defun complement (function)
  (lambda (&rest args) (not (apply function args))))

(defun constantly (value)
  (lambda (&rest args) value))

;;; ============================================================
;;; MAP-INTO
;;; ============================================================

(defun map-into (result function &rest sequences)
  "Destructively modify RESULT by applying FUNCTION to elements of SEQUENCES."
  (when (null result) (return-from map-into nil))
  ;; For fill-pointer vectors, use total array capacity as bound
  (let* ((has-fp (and (vectorp result) (array-has-fill-pointer-p result)))
         (result-cap (if has-fp (array-total-size result) (length result)))
         (n 0))
    ;; Walk all sequences simultaneously, stop at shortest or result capacity
    (block outer
      (loop
        ;; Check bounds
        (when (>= n result-cap) (return-from outer nil))
        ;; Collect current args from each sequence
        (let ((args nil)
              (done nil))
          (dolist (seq sequences)
            (if (listp seq)
                (let ((tail (nthcdr n seq)))
                  (if (null tail)
                      (progn (setq done t) (return))
                      (push (car tail) args)))
                (if (>= n (length seq))
                    (progn (setq done t) (return))
                    (push (elt seq n) args))))
          (when done (return-from outer nil))
          (let ((val (apply function (nreverse args))))
            ;; Use aref to bypass fill-pointer check for write
            (if has-fp
                (setf (aref result n) val)
                (setf (elt result n) val))))
        (incf n)))
    ;; Update fill pointer to reflect number of elements written
    (when has-fp
      (setf (fill-pointer result) n))
    result))

;;; ============================================================
;;; MERGE
;;; ============================================================

(defun %seq-to-list (seq)
  (if (listp seq) seq
      (let ((result nil))
        (dotimes (i (length seq) (nreverse result))
          (push (elt seq i) result)))))

(defun merge (result-type seq1 seq2 predicate &key (key #'identity))
  (let* ((key (or key #'identity))
         (l1 (%seq-to-list seq1))
         (l2 (%seq-to-list seq2))
         (result nil))
    (loop
      (cond
        ((null l1)
         (setq result (nreconc result l2))
         (return))
        ((null l2)
         (setq result (nreconc result l1))
         (return))
        ((funcall predicate (funcall key (car l2)) (funcall key (car l1)))
         (push (car l2) result)
         (setq l2 (cdr l2)))
        (t
         (push (car l1) result)
         (setq l1 (cdr l1)))))
    (let ((cat (%map-rt-category result-type)))
      (cond
        ((eq cat :nil) nil)
        ((eq cat :null) (if result (error 'type-error :datum result :expected-type 'null) nil))
        ((eq cat :list) result)
        ((eq cat :string) (coerce result 'string))
        ((eq cat :bit-vector)
         ;; Check compound size constraint: (bit-vector n)
         (when (and (consp result-type) (consp (cdr result-type)) (integerp (cadr result-type))
                    (/= (length result) (cadr result-type)))
           (error 'type-error :datum result :expected-type result-type))
         (coerce result 'bit-vector))
        ((eq cat :vector)
         ;; Check compound size constraint: (vector * n)
         (when (and (consp result-type) (consp (cdr result-type)) (consp (cddr result-type))
                    (integerp (caddr result-type))
                    (/= (length result) (caddr result-type)))
           (error 'type-error :datum result :expected-type result-type))
         (coerce result 'vector))
        (t
         ;; Unknown/compound type (e.g. (or (vector t 5) (vector t 10)))
         (multiple-value-bind (sub-list ok1) (subtypep result-type 'list)
           (multiple-value-bind (sub-string ok2) (subtypep result-type 'string)
             (multiple-value-bind (sub-vector ok3) (subtypep result-type 'vector)
               (let ((res (cond
                            ((and ok1 sub-list) result)
                            ((and ok2 sub-string) (coerce result 'string))
                            ((and ok3 sub-vector) (coerce result 'vector))
                            (t (error 'type-error :datum result-type
                                      :expected-type '(or list vector))))))
                 (unless (typep res result-type)
                   (error 'type-error :datum res :expected-type result-type))
                 res)))))))))

;;; ============================================================
;;; MISMATCH
;;; ============================================================

;;; mismatch is implemented in C# (Runtime.MismatchFull)

;;; ============================================================
;;; FILL
;;; ============================================================

;;; fill is implemented in C# (Runtime.Fill) for performance

;;; Helper for (setf (subseq seq start end) new-seq)
(defun %set-subseq (sequence start end new-sequence)
  (let* ((len (length sequence))
         (e (or end len)))
    (do ((i start (1+ i))
         (j 0 (1+ j)))
        ((or (>= i e) (>= j (length new-sequence))))
      (setf (elt sequence i) (elt new-sequence j))))
  new-sequence)

;;; ============================================================
;;; MAKE-SEQUENCE
;;; ============================================================

(defun %typexpand-full (type)
  "Recursively expand deftype aliases until no further expansion possible.
Also expands element types within compound type specifiers like (VECTOR etype size)."
  ;; First expand the top-level alias
  (loop
    (let* ((result (typexpand-1 type))
           (expanded (car result))
           (did-expand (cdr result)))
      (if (eq did-expand 'nil)
          (return)
          (setq type expanded))))
  ;; For compound types like (VECTOR etype size), also expand the element type
  (when (and (consp type) (symbolp (car type)))
    (let ((head-name (symbol-name (car type))))
      (when (member head-name '("VECTOR" "ARRAY" "SIMPLE-ARRAY" "SIMPLE-VECTOR") :test #'string=)
        (when (and (consp (cdr type)) (not (eq (cadr type) '*)))
          (let ((expanded-etype (%typexpand-full (cadr type))))
            (setq type (list* (car type) expanded-etype (cddr type))))))))
  type)

(defun make-sequence (type length &key (initial-element nil))
  (let* ((type (%typexpand-full type))
         (base-type (if (consp type) (car type) type))
         (base-name (when (symbolp base-type) (symbol-name base-type))))
    (cond
      ;; LIST types
      ((member base-name '("LIST") :test #'string=)
       (make-list length :initial-element initial-element))
      ;; NULL: only length 0 is valid
      ((string= base-name "NULL")
       (if (= length 0) nil
           (error 'type-error :datum length :expected-type '(integer 0 0))))
      ;; CONS type: non-empty list (length=0 is invalid)
      ((string= base-name "CONS")
       (if (= length 0)
           (error 'type-error :datum length :expected-type '(integer 1 *))
           (make-list length :initial-element initial-element)))
      ;; String types
      ((member base-name '("STRING" "SIMPLE-STRING" "BASE-STRING" "SIMPLE-BASE-STRING") :test #'string=)
       ;; Check size constraint: (string n)
       (when (and (consp type) (consp (cdr type)) (integerp (cadr type)) (/= length (cadr type)))
         (error 'type-error :datum length :expected-type type))
       (make-string length :initial-element (or initial-element #\Space)))
      ;; Bit-vector types
      ((member base-name '("BIT-VECTOR" "SIMPLE-BIT-VECTOR") :test #'string=)
       ;; Check size constraint: (bit-vector n)
       (when (and (consp type) (consp (cdr type)) (integerp (cadr type)) (/= length (cadr type)))
         (error 'type-error :datum length :expected-type type))
       (make-array length :element-type 'bit :initial-element (or initial-element 0)))
      ;; Vector types (possibly with element type)
      ((member base-name '("VECTOR" "SIMPLE-VECTOR" "ARRAY" "SIMPLE-ARRAY") :test #'string=)
       ;; Check size constraint: (vector * n) or (vector etype n)
       (when (and (consp type) (consp (cdr type)) (consp (cddr type))
                  (integerp (caddr type)) (/= length (caddr type)))
         (error 'type-error :datum length :expected-type type))
       (let ((etype (if (and (consp type) (consp (cdr type))) (cadr type) t)))
         (if (or (eq etype t) (eq etype '*) (and (symbolp etype) (string= (symbol-name etype) "*")))
             (make-array length :initial-element initial-element)
             (make-array length :element-type etype :initial-element (or initial-element 0)))))
      ;; Sequence is abstract
      ((string= base-name "SEQUENCE")
       (error 'type-error :datum type :expected-type 'sequence))
      (t
       ;; Unknown type: check if it's some known class; otherwise TYPE-ERROR
       (cond
         ((subtypep type 'list)
          (make-list length :initial-element initial-element))
         ((subtypep type 'string)
          (make-string length :initial-element (or initial-element #\Space)))
         ((subtypep type 'vector)
          ;; OR compound vector types are ambiguous for construction
          (when (and (consp type)
                     (let ((h (car type)))
                       (and (symbolp h) (string= (symbol-name h) "OR"))))
            (error 'type-error :datum length :expected-type type))
          (make-array length :initial-element initial-element))
         (t (error 'type-error :datum type :expected-type 'sequence)))))))

;;; ============================================================
;;; REMOVE-DUPLICATES
;;; ============================================================

;;; remove-duplicates, delete-duplicates are implemented in C# (Runtime.RemoveDuplicatesFull)

;;; ============================================================
;;; CLOS initialization protocol
;;; ============================================================

;; change-class is a compiler primitive (see cil-compiler.lisp)

;;; ============================================================
;;; CONSTANTP
;;; ============================================================

(defun constantp (form &optional env)
  (declare (ignore env))
  (cond
    ((and (consp form) (eq (car form) 'quote)) t)
    ((symbolp form) (or (keywordp form) (eq form t) (eq form nil)
                        (symbol-constant-p form)))
    ((consp form) nil)
    (t t)))  ; self-evaluating: numbers, chars, strings, vectors, etc.

;;; ============================================================
;;; Byte operations (BYTE, LDB, DPB, MASK-FIELD, DEPOSIT-FIELD)
;;; ============================================================

(defun byte (size position) (cons size position))
(defun byte-size (bytespec) (car bytespec))
(defun byte-position (bytespec) (cdr bytespec))

(defun logandc1 (integer1 integer2) (logand (lognot integer1) integer2))
(defun logandc2 (integer1 integer2) (logand integer1 (lognot integer2)))
(defun logorc1 (integer1 integer2) (logior (lognot integer1) integer2))
(defun logorc2 (integer1 integer2) (logior integer1 (lognot integer2)))
(defun logeqv (&rest args)
  (cond ((null args) -1)
        ((null (cdr args))
         (if (integerp (car args))
             (car args)
             (error 'type-error :datum (car args) :expected-type 'integer)))
        (t (let ((r (apply #'logxor args)))
             (if (evenp (length args)) (lognot r) r)))))
(defun lognand (integer1 integer2) (lognot (logand integer1 integer2)))
(defun lognor (integer1 integer2) (lognot (logior integer1 integer2)))
(defun logcount (integer)
  (if (minusp integer)
      (logcount (lognot integer))
      (let ((n integer) (count 0))
        (loop while (not (zerop n)) do
          (setq n (logand n (- n 1)))
          (setq count (+ count 1)))
        count)))

(defun ldb (bytespec integer)
  "Extract SIZE bits of INTEGER at POSITION."
  (let ((size (byte-size bytespec))
        (pos  (byte-position bytespec)))
    (logand (ash integer (- pos))
            (1- (ash 1 size)))))

(defun ldb-test (bytespec integer)
  "Return T if LDB of BYTESPEC in INTEGER is nonzero."
  (not (zerop (ldb bytespec integer))))

(defun dpb (value bytespec integer)
  "Deposit VALUE (SIZE bits) into INTEGER at POSITION."
  (let* ((size (byte-size bytespec))
         (pos  (byte-position bytespec))
         (mask (ash (1- (ash 1 size)) pos)))
    (logior (logandc2 integer mask)
            (logand (ash value pos) mask))))

(defun mask-field (bytespec integer)
  "Return INTEGER with only the BYTESPEC field retained."
  (let* ((size (byte-size bytespec))
         (pos  (byte-position bytespec))
         (mask (ash (1- (ash 1 size)) pos)))
    (logand integer mask)))

(defun deposit-field (value bytespec integer)
  "Like DPB but value is already a field (not raw value)."
  (let* ((size (byte-size bytespec))
         (pos  (byte-position bytespec))
         (mask (ash (1- (ash 1 size)) pos)))
    (logior (logandc2 integer mask)
            (logand value mask))))

(defun %set-ldb (bytespec integer new-value)
  "Setter helper for (setf (ldb bytespec place) val) — returns new integer."
  (dpb new-value bytespec integer))

(defun %set-mask-field (bytespec integer new-value)
  "Setter helper for (setf (mask-field bytespec place) val) — returns new integer."
  (deposit-field new-value bytespec integer))

;;; --- get-setf-expansion (CL public function) ---
;;; Defined here (not in cil-macros.lisp) to avoid SBCL package lock.
;;; Calls the compiler-internal %get-setf-expansion which is defined in cil-macros.lisp.
(defun get-setf-expansion (place &optional env)
  (declare (ignore env))
  (%get-setf-expansion place))

;;; ===== Generic atomic operations on arbitrary CL places =====
;;;
;;; dotcl:compare-and-swap / atomic-incf / atomic-decf. Lock-based (a single
;;; global monitor) — correct among concurrent atomic operations, but NOT
;;; lock-free. Because these expand through GET-SETF-EXPANSION, every place SETF
;;; understands works uniformly: a special/lexical variable, CAR/CDR, SVREF/AREF,
;;; GETHASH, SLOT-VALUE, a struct slot, etc. The place subforms are captured to
;;; temporaries up front, so the read-compare-write runs under the lock on a
;;; consistent snapshot. For a single 64-bit counter the lock-free
;;; MAKE-ATOMIC-LONG primitive is the faster path.
;;;
;;; The DOTCL-package macro names are interned and registered at runtime rather
;;; than written with dotcl:: reader syntax, because the SBCL cross-compile host
;;; that reads this file has no DOTCL package — the same reason
;;; without-package-locks is registered from C#. Helper functions and the lock
;;; special variable stay unqualified (internal); only the user-facing macro
;;; names need to live in DOTCL.
(defvar *cas-lock* (%make-lock "dotcl-cas-global"))

(defun %cas-expand (place old new)
  ;; Returns the PRIOR value of PLACE (sb-ext / CCL / Interlocked.CompareExchange
  ;; convention), not a T/NIL success flag: success is (eql old ret), and a failed CAS
  ;; hands back the current value for the next retry with no separate (racy) re-read.
  ;;
  ;; The comparison is EQL, not EQ. SBCL specifies EQ, but its fixnums are
  ;; immediates, so EQ there behaves as a value comparison for exactly the objects
  ;; EQL adds: numbers and characters. Here they are boxed and only a small cache
  ;; is shared, so EQ would make a CAS on a counter start failing the moment it
  ;; left that cache -- code ported from sb-ext works up to a point and then
  ;; silently stops swapping. EQL restores the intended semantics; it costs one
  ;; type test per attempt, under a lock that already dominates it.
  (multiple-value-bind (temps vals stores setter getter) (get-setf-expansion place)
    (let ((s (car stores)) (o (gensym "OLD")) (n (gensym "NEW")) (cur (gensym "CUR")))
      `(let* (,@(mapcar #'list temps vals) (,o ,old) (,n ,new))
         (%acquire-lock *cas-lock* t)
         (unwind-protect
              (let ((,cur ,getter))
                (when (eql ,cur ,o) (let ((,s ,n)) ,setter))
                ,cur)
           (%release-lock *cas-lock*))))))

(defun %atomic-delta-expand (place delta op)
  (multiple-value-bind (temps vals stores setter getter) (get-setf-expansion place)
    (let ((s (car stores)) (d (gensym "D")) (r (gensym "NEW")))
      `(let* (,@(mapcar #'list temps vals) (,d ,delta))
         (%acquire-lock *cas-lock* t)
         (unwind-protect
              (let* ((,r (,op ,getter ,d)) (,s ,r)) ,setter ,r)
           (%release-lock *cas-lock*))))))

(let ((cas  (intern "COMPARE-AND-SWAP" "DOTCL"))
      (ainc (intern "ATOMIC-INCF" "DOTCL"))
      (adec (intern "ATOMIC-DECF" "DOTCL"))
      (pkg  (find-package "DOTCL")))
  (%register-macro-function-rt cas
    (lambda (form env) (declare (ignore env))
      (%cas-expand (cadr form) (caddr form) (cadddr form))))
  (%register-macro-function-rt ainc
    (lambda (form env) (declare (ignore env))
      (%atomic-delta-expand (cadr form) (if (cddr form) (caddr form) 1) '+)))
  (%register-macro-function-rt adec
    (lambda (form env) (declare (ignore env))
      (%atomic-delta-expand (cadr form) (if (cddr form) (caddr form) 1) '-)))
  (export (list cas ainc adec) pkg))

;;; --- Missing numeric functions ---

(defun signum (n)
  (cond
    ((complexp n)
     (if (zerop n) n
       (let ((a (abs n)))
         ;; Coerce abs to match the float type of the complex parts
         (cond
           ((typep (realpart n) 'single-float)
            (/ n (coerce a 'single-float)))
           ((typep (realpart n) 'double-float)
            (/ n a))
           ;; Integer/rational complex: result should be single-float complex
           (t (/ (complex (coerce (realpart n) 'single-float)
                          (coerce (imagpart n) 'single-float))
                 (coerce a 'single-float)))))))
    ((rationalp n) (cond ((plusp n) 1) ((minusp n) -1) (t 0)))
    ((typep n 'single-float) (cond ((plusp n) 1.0) ((minusp n) -1.0) (t 0.0)))
    ((typep n 'double-float) (cond ((plusp n) 1.0d0) ((minusp n) -1.0d0) (t 0.0d0)))
    (t (error "SIGNUM: not a number: ~S" n))))

(defun scale-float (float integer)
  (* float (expt 2 integer)))

(defun %simplest-rational-between (lo hi)
  "The smallest-denominator rational R with LO <= R <= HI, given 0 < LO <= HI.
   Continued-fraction (Stern-Brocot) descent: the simplest rational in an interval
   is either the smallest integer it contains, or FLOOR(LO) + 1/(simplest rational
   in the reciprocal of the fractional interval)."
  (labels ((simplest (lo hi)
             (multiple-value-bind (fl rem) (floor lo)
               (cond
                 ((zerop rem) fl)                 ; LO is an integer — simplest possible
                 ((< fl (floor hi)) (1+ fl))      ; integer FL+1 lies in (LO, HI]
                 (t (+ fl (/ (simplest (/ (- hi fl)) (/ (- lo fl))))))))))
    (simplest lo hi)))

(defun rationalize (x)
  "Return the SIMPLEST rational that rounds to X (CLHS): the smallest-denominator
   rational within X's rounding interval — not RATIONAL's exact fraction. E.g.
   (rationalize 3.2d0) => 16/5, (rationalize 0.1d0) => 1/10."
  (cond
    ((rationalp x) x)
    ((floatp x)
     (if (zerop x)
         0
         (multiple-value-bind (m e s) (integer-decode-float x)
           ;; X's magnitude is M*2^E; the rounding interval is a half-ulp (2^(E-1))
           ;; on each side. Find the simplest rational in it, then re-apply the sign.
           (let* ((ulp (if (minusp e) (/ 1 (ash 1 (- e))) (ash 1 e)))
                  (val (* m ulp))
                  (half (/ ulp 2))
                  ;; The float just below X has half the ulp when X sits on a
                  ;; power-of-2 mantissa boundary (M minimal), so the lower rounding
                  ;; boundary is half as far.
                  (lower (if (= m (ash 1 (1- (float-precision x)))) (/ half 2) half))
                  (cand (%simplest-rational-between (- val lower) (+ val half)))
                  (r (if (minusp s) (- cand) cand)))
             ;; The result must round back to X (CLHS). If a round-to-even boundary
             ;; case let the interval's simplest rational land on a neighbor, fall
             ;; back to the exact rational (which always rounds to X).
             (if (= (float r x) x) r (rational x))))))
    (t (rational x))))

(defun conjugate (z)
  (if (complexp z)
      (complex (realpart z) (- (imagpart z)))
      z))

;;; ===== DOCUMENTATION generic function (CLHS) =====

;; Global storage: (cons object doc-type) → string
(defvar *%pprint-level* 0)

(defvar *compilation-unit-depth* 0)
(defvar *deferred-compilation-warnings* nil)

(defvar *documentation-table* (make-hash-table :test #'equal :synchronized t))

;; Helper to make a key for the documentation table
(defun %doc-key (obj doc-type)
  (cons obj doc-type))

;; Helper to get documentation (returns single value, not gethash's two values)
(defun %get-doc (obj doc-type)
  ;; A slot definition carries its own documentation. AMOP passes it to
  ;; EFFECTIVE-SLOT-DEFINITION-CLASS, and an effective slot definition is built
  ;; rather than defined, so nothing ever registered it in the table below.
  ;; %SLOT-DEF-DOCUMENTATION-OR-NIL answers NIL for everything else, which keeps
  ;; this file free of a reference to the MOP package -- it is read by the host
  ;; Lisp during the cross compile, where that package does not exist.
  (or (%slot-def-documentation-or-nil obj)
      (values (gethash (%doc-key obj doc-type) *documentation-table*))))

;; --- DOCUMENTATION methods ---

(defgeneric documentation (x doc-type))

;; Default: return NIL
(defmethod documentation ((x t) (doc-type t))
  (%get-doc x doc-type))

;; (documentation <function> 't) — function object documentation
(defmethod documentation ((x function) (doc-type (eql t)))
  (%get-doc x t))

;; (documentation <function> 'function)
(defmethod documentation ((x function) (doc-type (eql 'function)))
  (documentation x t))

;; (documentation <symbol> 'function) — user-set docs first, then built-in docs
;; registered via [LispDoc]/SetFunctionDoc (mirrors the variable path) (#25).
(defmethod documentation ((x symbol) (doc-type (eql 'function)))
  (or (%get-doc x 'function)
      (%get-function-documentation x)))

;; (documentation <symbol> 'variable)
(defmethod documentation ((x symbol) (doc-type (eql 'variable)))
  (or (%get-doc x 'variable)
      (%get-variable-documentation x)))

;; (documentation <symbol> 'type)
(defmethod documentation ((x symbol) (doc-type (eql 'type)))
  (%get-doc x 'type))

;; (documentation <symbol> 'structure)
(defmethod documentation ((x symbol) (doc-type (eql 'structure)))
  (%get-doc x 'structure))

;; (documentation <symbol> 'compiler-macro)
(defmethod documentation ((x symbol) (doc-type (eql 'compiler-macro)))
  (%get-doc x 'compiler-macro))

;; (documentation <symbol> 'setf)
(defmethod documentation ((x symbol) (doc-type (eql 'setf)))
  (%get-doc x 'setf))

;; (documentation <list> 'function) — for (setf foo)
(defmethod documentation ((x list) (doc-type (eql 'function)))
  (%get-doc x 'function))

;; (documentation <list> 'compiler-macro)
(defmethod documentation ((x list) (doc-type (eql 'compiler-macro)))
  (%get-doc x 'compiler-macro))

;; (documentation <package> 't)
(defmethod documentation ((x package) (doc-type (eql t)))
  (%get-doc x t))

;; --- (SETF DOCUMENTATION) methods ---

(defgeneric (setf documentation) (new-value x doc-type))

;; Default setter
(defmethod (setf documentation) ((new-value t) (x t) (doc-type t))
  (if new-value
      (setf (gethash (%doc-key x doc-type) *documentation-table*) new-value)
      (remhash (%doc-key x doc-type) *documentation-table*))
  new-value)

;; (setf (documentation <function> 't) val)
(defmethod (setf documentation) ((new-value t) (x function) (doc-type (eql t)))
  (if new-value
      (setf (gethash (%doc-key x t) *documentation-table*) new-value)
      (remhash (%doc-key x t) *documentation-table*))
  new-value)

;; (setf (documentation <function> 'function) val)
(defmethod (setf documentation) ((new-value t) (x function) (doc-type (eql 'function)))
  (setf (documentation x t) new-value))

;; (setf (documentation <symbol> 'function) val)
(defmethod (setf documentation) ((new-value t) (x symbol) (doc-type (eql 'function)))
  (if new-value
      (setf (gethash (%doc-key x 'function) *documentation-table*) new-value)
      (remhash (%doc-key x 'function) *documentation-table*))
  new-value)

;; (setf (documentation <symbol> 'variable) val)
(defmethod (setf documentation) ((new-value t) (x symbol) (doc-type (eql 'variable)))
  (if new-value
      (setf (gethash (%doc-key x 'variable) *documentation-table*) new-value)
      (remhash (%doc-key x 'variable) *documentation-table*))
  new-value)

;; (setf (documentation <symbol> 'type) val)
(defmethod (setf documentation) ((new-value t) (x symbol) (doc-type (eql 'type)))
  (if new-value
      (setf (gethash (%doc-key x 'type) *documentation-table*) new-value)
      (remhash (%doc-key x 'type) *documentation-table*))
  new-value)

;; (setf (documentation <symbol> 'structure) val)
(defmethod (setf documentation) ((new-value t) (x symbol) (doc-type (eql 'structure)))
  (if new-value
      (setf (gethash (%doc-key x 'structure) *documentation-table*) new-value)
      (remhash (%doc-key x 'structure) *documentation-table*))
  new-value)

;; (setf (documentation <symbol> 'compiler-macro) val)
(defmethod (setf documentation) ((new-value t) (x symbol) (doc-type (eql 'compiler-macro)))
  (if new-value
      (setf (gethash (%doc-key x 'compiler-macro) *documentation-table*) new-value)
      (remhash (%doc-key x 'compiler-macro) *documentation-table*))
  new-value)

;; (setf (documentation <symbol> 'setf) val)
(defmethod (setf documentation) ((new-value t) (x symbol) (doc-type (eql 'setf)))
  (if new-value
      (setf (gethash (%doc-key x 'setf) *documentation-table*) new-value)
      (remhash (%doc-key x 'setf) *documentation-table*))
  new-value)

;; (setf (documentation <list> 'function) val)
(defmethod (setf documentation) ((new-value t) (x list) (doc-type (eql 'function)))
  (if new-value
      (setf (gethash (%doc-key x 'function) *documentation-table*) new-value)
      (remhash (%doc-key x 'function) *documentation-table*))
  new-value)

;; (setf (documentation <list> 'compiler-macro) val)
(defmethod (setf documentation) ((new-value t) (x list) (doc-type (eql 'compiler-macro)))
  (if new-value
      (setf (gethash (%doc-key x 'compiler-macro) *documentation-table*) new-value)
      (remhash (%doc-key x 'compiler-macro) *documentation-table*))
  new-value)

;; (setf (documentation <package> 't) val)
(defmethod (setf documentation) ((new-value t) (x package) (doc-type (eql t)))
  (if new-value
      (setf (gethash (%doc-key x t) *documentation-table*) new-value)
      (remhash (%doc-key x t) *documentation-table*))
  new-value)

;; (documentation <symbol> 't) — check function, then variable
(defmethod documentation ((x symbol) (doc-type (eql t)))
  (%get-doc x t))

;; Install docstrings for the DOTNET: built-in functions (#25). The C# registration
;; collects (symbol . docstring) pairs; apply them now that DOCUMENTATION is defined.
(dolist (%dn-doc (%dotnet-doc-alist))
  (setf (documentation (car %dn-doc) 'function) (cdr %dn-doc)))

;;; --- ensure-generic-function ---
(defun %ensure-gf-required-params (ll)
  "The required parameter names of lambda-list LL (before any lambda-list keyword)."
  (let ((result nil))
    (dolist (x ll (nreverse result))
      (when (member x '(&rest &optional &key &allow-other-keys &body &aux))
        (return (nreverse result)))
      (push x result))))

(defun %ensure-gf-apply-lambda-list (gf lambda-list apo-params)
  "Apply LAMBDA-LIST and ARGUMENT-PRECEDENCE-ORDER (APO-PARAMS, a list of param
names or nil) to GF via %set-gf-lambda-list-info. Performs congruence checking
against existing methods and invalidates the dispatch cache (CLHS 7.6.4)."
  (let* ((required (%ensure-gf-required-params lambda-list))
         (state :required)
         (req 0) (opt 0) (rest-p nil) (key-p nil) (aok-p nil) (kw-names nil))
    (dolist (p lambda-list)
      (cond
        ((eq p '&optional) (setf state :optional))
        ((member p '(&rest &body)) (setf state :rest))
        ((eq p '&key) (setf key-p t) (setf state :key))
        ((eq p '&allow-other-keys) (setf aok-p t))
        ((eq p '&aux) (setf state :aux))
        (t (case state
             (:required (incf req))
             (:optional (incf opt))
             (:rest (setf rest-p t))
             (:key (push (if (consp p) (if (consp (car p)) (caar p) (car p)) p) kw-names))
             (otherwise nil)))))
    ;; :argument-precedence-order must be a permutation of the required params.
    (let ((apo-indices nil))
      (when apo-params
        (dolist (p apo-params)
          (unless (member p required) (error 'program-error)))
        (dolist (p required)
          (unless (member p apo-params) (error 'program-error)))
        (setf apo-indices (mapcar (lambda (p) (position p required)) apo-params)))
      (%set-gf-lambda-list-info gf req opt
                                (if rest-p t nil) (if key-p t nil) (if aok-p t nil)
                                (nreverse kw-names)
                                apo-indices lambda-list))))

(defun ensure-generic-function (name &rest args)
  ;; Odd number of keyword args → program-error
  (when (oddp (length args))
    (error 'program-error))
  (let ((lambda-list-p nil)
        (lambda-list nil)
        (apo-p nil)
        (apo-params nil))
    ;; Manual keyword parsing
    (do ((rest args (cddr rest)))
        ((null rest))
      (cond
        ((eq (car rest) :lambda-list)
         (setf lambda-list (cadr rest) lambda-list-p t))
        ((eq (car rest) :argument-precedence-order)
         (setf apo-params (cadr rest) apo-p t))))
    ;; If already fbound: must be a GF; apply lambda-list / precedence updates
    ;; in place (CLHS — ensure-generic-function reinitializes the existing GF).
    (if (fboundp name)
        (let ((fn (fdefinition name)))
          (unless (typep fn 'generic-function) (error 'program-error))
          (when (or lambda-list-p apo-p)
            (%ensure-gf-apply-lambda-list fn lambda-list apo-params))
          fn)
        ;; Create new GF
        (let* ((ll (if lambda-list-p lambda-list '()))
               (arity (length (%ensure-gf-required-params ll)))
               (gf (%make-gf name arity)))
          (%register-gf name gf)
          (setf (fdefinition name) gf)
          (when (or lambda-list-p apo-p)
            (%ensure-gf-apply-lambda-list gf ll apo-params))
          gf))))

;;; --- CLHS 3.2.4.2: make-load-form protocol ---

(defgeneric make-load-form (object &optional environment))

(defun make-load-form-saving-slots (object &key slot-names environment allow-other-keys)
  (declare (ignore environment allow-other-keys))
  (let* ((class (class-of object))
         (all-slots (class-slots class))
         (slots (if slot-names
                    (remove-if-not (lambda (sd)
                                     (member (slot-definition-name sd) slot-names))
                                   all-slots)
                    all-slots))
         (bound-slots (remove-if-not (lambda (sd)
                                       (slot-boundp object (slot-definition-name sd)))
                                     slots))
         (new-var (gensym "NEW")))
    ;; Return a single combined creation form with nil init form (CLHS-compliant).
    ;; Using allocate-instance + slot-value setf in one let form avoids the need
    ;; for the FASL emitter to handle a separate init form for struct objects.
    (values
     `(let ((,new-var (allocate-instance (find-class ',(class-name class)))))
        ,@(mapcar (lambda (sd)
                    (let ((name (slot-definition-name sd)))
                      `(setf (slot-value ,new-var ',name)
                             ',(slot-value object name))))
                  bound-slots)
        ,new-var)
     nil)))

;;; --- MOP: allow AMOP initargs for standard-generic-function and standard-method ---
;;; Per AMOP, make-instance 'standard-generic-function and 'standard-method accept
;;; these keyword args. The :before methods accept &allow-other-keys so that
;;; ValidateInitargs passes; the actual protocol implementation is a stub.

(defmethod initialize-instance :before ((gf standard-generic-function)
                                        &rest all-keys
                                        &key lambda-list argument-precedence-order
                                        declarations documentation method-class
                                        method-combination name
                                        &allow-other-keys)
  (declare (ignore all-keys lambda-list argument-precedence-order declarations
                   documentation method-class method-combination name)))

(defmethod initialize-instance :before ((m standard-method)
                                        &rest all-keys
                                        &key qualifiers lambda-list specializers
                                        function documentation
                                        &allow-other-keys)
  (declare (ignore all-keys qualifiers lambda-list specializers function documentation)))


;;; --- typed dotnet:invoke -> direct callvirt (compiler-macro surface) ---
;; When the receiver (and every argument) carries a static .NET type via
;; (the (dotnet "Type.FullName") expr), lower (dotnet:invoke ...) to the
;; %dotnet-call-direct codegen target (resolved overload + direct callvirt, no
;; InvokeMember). Otherwise decline so the normal dynamic interop path is used.
;; Registered at boot under a (find-package "DOTNET") guard because the SBCL
;; cross-compile host has no DOTNET package / DOTNET:INVOKE symbol.
(defun %dotnet-the-type (x)
  "If X is (the (dotnet \"T\") EXPR), return (cons \"T\" EXPR), else NIL."
  (and (consp x) (consp (cdr x)) (consp (cddr x)) (null (cdddr x))
       (symbolp (car x)) (string= (symbol-name (car x)) "THE")
       (let ((spec (cadr x)))
         (and (consp spec) (consp (cdr spec)) (null (cddr spec))
              (symbolp (car spec)) (string= (symbol-name (car spec)) "DOTNET")
              (stringp (cadr spec))
              (cons (cadr spec) (caddr x))))))

(defun %dotnet-in-pkg (sym name)
  "True when symbol SYM is named NAME in the DOTNET package."
  (and (symbolp sym) (string= (symbol-name sym) name)
       (let ((p (symbol-package sym)))
         (and p (string= (package-name p) "DOTNET")))))

(defun %dotnet-static-type (x &optional env)
  "If X statically denotes a typed .NET value, return (cons \"T\" EXPR) where
EXPR evaluates to a .NET object of type \"T\". Recognizes:
  (the (dotnet \"T\") E)   -> E is already a .NET object  (cons \"T\" E)
  (dotnet:box E \"T\")      -> the box form yields a LispDotNetBoxed of T
  (dotnet:new \"T\" ...)    -> the new form yields a LispDotNetObject of T
  bare symbol VAR         -> when ENV maps VAR's name to a type (a let/let*-bound
                             local whose init was one of the above); the symbol
                             itself evaluates to the typed object  (cons \"T\" VAR)
  (dotnet:invoke R \"M\" ...) -> when R is itself statically typed and M's return
                             type is a directable .NET reference/value type, the
                             whole call yields a LispDotNetObject of that type
                             (method chaining)  (cons RETURN-TYPE FORM)
For box/new the whole form is the EXPR (it produces the wrapped object), so the
typed direct call works without an explicit THE (dotcl/dotcl#42). ENV is the .NET
type environment supplied by the compiler (compiler/cil-compiler.lisp:
DOTNET-VALID-TYPED-LOCALS), an alist (name-string . type-string)."
  (or (%dotnet-the-type x)
      (and (symbolp x) env
           (let ((ty (cdr (assoc (symbol-name x) env :test #'string=))))
             (and ty (cons ty x))))
      (and (consp x)
           (cond
             ;; (dotnet:box E "T") — literal type in 3rd position
             ((and (%dotnet-in-pkg (car x) "BOX")
                   (consp (cdr x)) (consp (cddr x)) (null (cdddr x))
                   (stringp (caddr x)))
              (cons (caddr x) x))
             ;; (dotnet:new "T" ...) — literal type in 2nd position
             ((and (%dotnet-in-pkg (car x) "NEW")
                   (consp (cdr x)) (stringp (cadr x)))
              (cons (cadr x) x))
             ;; (dotnet:invoke R "M" ...) — typed-return propagation
             ((%dotnet-in-pkg (car x) "INVOKE")
              (let ((rty (%dotnet-invoke-return-type x env)))
                (and rty (cons rty x))))
             (t nil)))))

(defun %dotnet-invoke-return-type (x env)
  "If X is (dotnet:invoke R \"M\" ARG...) with a statically typed receiver R and
fully statically typed arguments, and M's resolved return type is directable
(marshals to a LispDotNetObject — see runtime %DOTNET-METHOD-RETURN-TYPE), return
that return-type string; else NIL. Mutually recursive with %DOTNET-STATIC-TYPE so
chains of any depth resolve. The inner call need not itself lower to a direct
callvirt: both the direct and the dynamic path return a LispDotNetObject of the
return type, so the OUTER direct dispatch is sound either way."
  (let ((recv (cadr x)) (method (caddr x)) (args (cdddr x)))
    (and (stringp method)
         (let ((rt (%dotnet-static-type recv env)))
           (and rt
                (let ((param-types '()) (ok t))
                  (dolist (a args)
                    (let ((at (%dotnet-static-type a env)))
                      (if at (push (car at) param-types) (setf ok nil))))
                  (and ok
                       (%dotnet-method-return-type
                        (car rt) method (reverse param-types)))))))))

(defun %dotnet-invoke-direct-cm (form env)
  "Compiler macro for DOTNET:INVOKE: rewrite a fully type-declared call to
%DOTNET-CALL-DIRECT; decline (return FORM) otherwise. The receiver and each
argument may carry their type via THE, DOTNET:BOX, or DOTNET:NEW, or be a bare
local variable whose type is known from its let/let* init form (ENV, the compiler-
supplied .NET type environment). The lowering is optimistic: the assembler resolves
the exact overload and silently falls back to the dynamic path when it can't
(unresolvable / runtime-defined type, value-type receiver, or no matching
overload), so this never changes behaviour, only speed."
  (let ((recv (cadr form)) (method (caddr form)) (args (cdddr form)))
    (let ((rt (and (stringp method) (%dotnet-static-type recv env))))
      (if (not rt)
          form
          (let ((param-types '()) (arg-exprs '()) (ok t))
            (dolist (a args)
              (let ((at (%dotnet-static-type a env)))
                (if at
                    (progn (push (car at) param-types) (push (cdr at) arg-exprs))
                    (setf ok nil))))
            (if ok
                `(%dotnet-call-direct ,(car rt) ,method ,(reverse param-types)
                                      ,(cdr rt) ,@(reverse arg-exprs))
                form))))))

(let ((pkg (find-package "DOTNET")))
  (when pkg
    (let ((sym (find-symbol "INVOKE" pkg)))
      (when sym (%register-compiler-macro-rt sym #'%dotnet-invoke-direct-cm)))))

;;; dotnet:handler-bind — handler-bind that dispatches on specific .NET exception
;;; types (dotcl/dotcl#45). Each clause is ("Type.Name" (var) body...). When a
;;; condition wraps a raw .NET exception whose CLR type is the named type or a
;;; subtype (dotnet:exception-typep), the matching clause runs; the clause body is
;;; expected to transfer control (return-from / invoke-restart) as in cl:handler-bind.
;;; Non-matching / non-.NET conditions propagate.
;;;
;;; Registered at load time via find-package/intern (no literal dotnet: prefix) so
;;; the SBCL cross-compile host — which has no DOTNET package and would otherwise
;;; read-suppress a #+dotcl form right out of the core — compiles it fine; it runs
;;; when the built core loads and DOTNET exists. The resolved exception-typep symbol
;;; is spliced into the expansion, so the macro body needs no dotnet: literal either.
(let ((pkg (find-package "DOTNET")))
  (when pkg
    (let ((hb  (intern "HANDLER-BIND" pkg))
          (etp (find-symbol "EXCEPTION-TYPEP" pkg)))
      (when etp
        (export hb pkg)
        (setf (gethash hb *macros*)
              (lambda (form)
                ;; form = (dotnet:handler-bind (clause ...) body ...)
                (let ((clauses (cadr form))
                      (body (cddr form))
                      (cvar (gensym "DNHB-C")))
                  `(handler-bind
                       ((error (lambda (,cvar)
                                 (cond
                                   ,@(loop for cl in clauses
                                           collect `((,etp ,cvar ,(car cl))
                                                     (let ((,(caadr cl) ,cvar))
                                                       ,@(cddr cl))))))))
                     ,@body))))))))

;;; dotnet:-> — a left-to-right member chain, so nested interop reads in call order:
;;;   (dotnet:-> uri "Host" ("Substring" 0 7) "ToUpper")
;;; instead of the inside-out
;;;   (dotnet:invoke (dotnet:invoke (dotnet:invoke uri "Host") "Substring" 0 7) "ToUpper")
;;; A step is a member name, or (member-name arg...) to pass arguments. Property and
;;; field reads need no getter prefix — dotnet:invoke resolves those too — and the
;;; whole chain is a place: (setf (dotnet:-> sb "Capacity") 64).
;;;
;;; dotnet:doto — apply several members to ONE object and return it:
;;;   (dotnet:doto sb ("Append" "a") ("Append" "b"))
;;;
;;; Member names are strings because the Lisp reader upcases bare symbols while .NET
;;; member names are case-sensitive; a symbol is accepted and contributes its name
;;; verbatim, so |Host| works and HOST (from bare host) correctly does not.
;;;
;;; Registered at load time via find-package/intern, like dotnet:handler-bind above:
;;; the SBCL cross-compile host has no DOTNET package.
(defun %dotnet-member-name (x)
  "Member designator in a dotnet chain step: a string verbatim, a symbol's name."
  (cond ((stringp x) x)
        ((symbolp x) (symbol-name x))
        (t x)))

(defun %dotnet-chain-step (invoke-sym target step)
  "One chain step applied to TARGET: (INVOKE target \"Name\" arg...)."
  (if (consp step)
      (list* invoke-sym target (%dotnet-member-name (car step)) (cdr step))
      (list invoke-sym target (%dotnet-member-name step))))

(let ((pkg (find-package "DOTNET")))
  (when pkg
    (let ((chain (intern "->" pkg))
          (doto (intern "DOTO" pkg))
          (inv (find-symbol "INVOKE" pkg)))
      (when inv
        (export chain pkg)
        (export doto pkg)
        (setf (gethash chain *macros*)
              (lambda (form)
                (let ((expr (cadr form)))
                  (dolist (step (cddr form) expr)
                    (setf expr (%dotnet-chain-step inv expr step))))))
        (setf (gethash doto *macros*)
              (lambda (form)
                (let ((obj (gensym "DOTO")))
                  `(let ((,obj ,(cadr form)))
                     ,@(mapcar (lambda (step) (%dotnet-chain-step inv obj step))
                               (cddr form))
                     ,obj))))))))

;;; ============================================================
;;; MACROEXPAND-ALL (exported as dotcl-cltl2:macroexpand-all)
;;; ============================================================
;;; A code walker that expands every macro form in FORM, including macros and
;;; symbol macros introduced by local MACROLET / SYMBOL-MACROLET. MACROEXPAND-1
;;; already reads a reified environment shaped (macros-ht . symbol-macros-ht)
;;; keyed by symbol NAME, which is what the compiler hands to &environment, so
;;; the walker carries tables of that shape and shadows them per scope.
;;;
;;; Scope tracking is what a structural walker cannot do: a lexical variable
;;; shadows a symbol macro of the same name, and FLET/LABELS shadow a global
;;; macro, so both must remove the binding on the way down.

(defun %mea-copy (ht)
  (let ((new (make-hash-table :test 'equal)))
    (when ht (maphash (lambda (k v) (setf (gethash k new) v)) ht))
    new))

(defun %mea-shadow (ht names)
  ;; Lexical bindings hide a symbol macro / macro of the same name.
  (if (null names)
      ht
      (let ((new (%mea-copy ht)))
        (dolist (n names new)
          (when (symbolp n) (remhash (symbol-name n) new))))))

(defun %mea-ll-vars (ll)
  ;; Variables a lambda list binds, so they can shadow symbol macros.
  (let ((vars '()))
    (dolist (x ll (nreverse vars))
      (cond ((member x '(&optional &rest &key &aux &allow-other-keys &body
                         &whole &environment)))
            ((symbolp x) (push x vars))
            ((consp x)
             (let ((head (car x)))
               (cond ((symbolp head) (push head vars))
                     ;; ((:key var) default svar)
                     ((and (consp head) (symbolp (cadr head))) (push (cadr head) vars))))
             (when (and (cddr x) (symbolp (caddr x))) (push (caddr x) vars)))))))

(defun %mea-strip-env (ll)
  ;; Drop &environment VAR from a macrolet lambda list; the walker has no
  ;; compiler environment object to pass, and CLHS lets it be absent.
  (let ((out '()) (rest ll))
    (loop while rest do
      (if (eq (car rest) '&environment)
          (setq rest (cddr rest))
          (progn (push (car rest) out) (setq rest (cdr rest)))))
    (nreverse out)))

(defun %mea-macrolet-expander (name ll body)
  ;; Build the expander MACROEXPAND-1 will call: it receives the whole form.
  (let ((whole (gensym "WHOLE"))
        (clean (%mea-strip-env ll)))
    (declare (ignorable name))
    (if (eq (car clean) '&whole)
        (let ((wvar (cadr clean)))
          (eval (list 'lambda (list whole)
                      (list 'let (list (list wvar whole))
                            (list* 'destructuring-bind (cddr clean)
                                   (list 'cdr whole) body)))))
        (eval (list 'lambda (list whole)
                    (list* 'destructuring-bind clean (list 'cdr whole) body))))))

(defun %mea-body (body macros symbol-macros fns)
  ;; Walk a body, leaving (declare ...) forms untouched.
  (mapcar (lambda (f)
            (if (and (consp f) (eq (car f) 'declare))
                f
                (%mea f macros symbol-macros fns)))
          body))

(defun %mea-list (forms macros symbol-macros fns)
  (mapcar (lambda (f) (%mea f macros symbol-macros fns)) forms))

(defun %mea-lambda-tail (rest macros symbol-macros fns)
  ;; (lambda-list . body) shared by LAMBDA, FLET/LABELS definitions.
  (let* ((ll (car rest))
         (vars (%mea-ll-vars ll))
         (sm (%mea-shadow symbol-macros vars)))
    (cons ll (%mea-body (cdr rest) macros sm fns))))

(defun %mea (form macros symbol-macros fns)
  (cond
    ;; A symbol may be a symbol macro, unless lexically shadowed (handled by
    ;; the callers that bind variables).
    ((symbolp form)
     (if (and form (not (eq form t)) (not (keywordp form)))
         (multiple-value-bind (exp expanded)
             (macroexpand-1 form (cons macros symbol-macros))
           (if expanded (%mea exp macros symbol-macros fns) form))
         form))
    ((atom form) form)
    (t
     (let ((head (car form)))
       ;; Expand macro calls first, but never a name shadowed by a local
       ;; function binding (FLET/LABELS beat a global macro of the same name).
       (when (and (symbolp head) (not (member head fns)))
         (multiple-value-bind (exp expanded)
             (macroexpand-1 form (cons macros symbol-macros))
           (when expanded
             (return-from %mea (%mea exp macros symbol-macros fns)))))
       (case head
         ((quote go declare) form)
         ((function)
          (if (and (consp (cadr form)) (eq (car (cadr form)) 'lambda))
              (list 'function (cons 'lambda (%mea-lambda-tail (cdr (cadr form))
                                                              macros symbol-macros fns)))
              form))
         ((lambda) (cons 'lambda (%mea-lambda-tail (cdr form) macros symbol-macros fns)))
         ((let let*)
          (let* ((binds (cadr form))
                 (walked (mapcar (lambda (b)
                                   (if (consp b)
                                       (list (car b) (%mea (cadr b) macros symbol-macros fns))
                                       b))
                                 binds))
                 (vars (mapcar (lambda (b) (if (consp b) (car b) b)) binds))
                 (sm (%mea-shadow symbol-macros vars)))
            (list* head walked (%mea-body (cddr form) macros sm fns))))
         ((flet labels)
          (let* ((defs (cadr form))
                 (names (mapcar (lambda (d) (car d)) defs))
                 ;; LABELS definitions see each other; FLET's do not.
                 (inner-fns (if (eq head 'labels) (append names fns) fns))
                 (walked (mapcar (lambda (d)
                                   (cons (car d)
                                         (%mea-lambda-tail (cdr d) macros symbol-macros
                                                           inner-fns)))
                                 defs))
                 (body-macros (%mea-shadow macros names)))
            (list* head walked
                   (%mea-body (cddr form) body-macros symbol-macros
                              (append names fns)))))
         ((macrolet)
          (let ((new (%mea-copy macros)))
            (dolist (d (cadr form))
              (setf (gethash (symbol-name (car d)) new)
                    (%mea-macrolet-expander (car d) (cadr d) (cddr d))))
            ;; The macrolet itself disappears: its body is walked with the
            ;; definitions in scope, which is the point of the whole exercise.
            (let ((walked (%mea-body (cddr form) new symbol-macros fns)))
              (if (= (length walked) 1) (car walked) (cons 'progn walked)))))
         ((symbol-macrolet)
          (let ((new (%mea-copy symbol-macros)))
            (dolist (d (cadr form))
              (setf (gethash (symbol-name (car d)) new) (cadr d)))
            (let ((walked (%mea-body (cddr form) macros new fns)))
              (if (= (length walked) 1) (car walked) (cons 'progn walked)))))
         ((setq)
          ;; A symbol macro in the place turns SETQ into SETF (CLHS 5.1.2.4).
          (let ((out '()) (rest (cdr form)))
            (loop while rest do
              (let* ((place (car rest))
                     (value (%mea (cadr rest) macros symbol-macros fns))
                     (sm (and (symbolp place)
                              (gethash (symbol-name place) symbol-macros))))
                (if sm
                    (push (%mea (list 'setf sm value) macros symbol-macros fns) out)
                    (progn (push place out) (push value out))))
              (setq rest (cddr rest)))
            (let ((parts (nreverse out)))
              (if (and parts (consp (car parts)))
                  (if (= (length parts) 1) (car parts) (cons 'progn parts))
                  (cons 'setq parts)))))
         ((block catch throw return-from the multiple-value-call
           multiple-value-prog1 unwind-protect progn locally eval-when if)
          ;; Forms whose first subform is a name/keyword rather than a form.
          (case head
            ((block return-from)
             (list* head (cadr form) (%mea-list (cddr form) macros symbol-macros fns)))
            ((the) (list 'the (cadr form) (%mea (caddr form) macros symbol-macros fns)))
            ((eval-when)
             (list* 'eval-when (cadr form) (%mea-body (cddr form) macros symbol-macros fns)))
            ((locally) (list* 'locally (%mea-body (cdr form) macros symbol-macros fns)))
            (t (cons head (%mea-list (cdr form) macros symbol-macros fns)))))
         ((tagbody)
          (cons 'tagbody
                (mapcar (lambda (f)
                          (if (or (symbolp f) (integerp f))
                              f
                              (%mea f macros symbol-macros fns)))
                        (cdr form))))
         ((progv)
          (list* 'progv (%mea-list (cdr form) macros symbol-macros fns)))
         ((load-time-value)
          (list* 'load-time-value (%mea (cadr form) macros symbol-macros fns) (cddr form)))
         (t
          ;; Ordinary call. ((lambda ...) args) keeps its operator walked too.
          (if (consp head)
              (cons (%mea head macros symbol-macros fns)
                    (%mea-list (cdr form) macros symbol-macros fns))
              (cons head (%mea-list (cdr form) macros symbol-macros fns)))))))))

(defun %macroexpand-all (form &optional env)
  "Expand every macro in FORM, including ones from local MACROLET /
SYMBOL-MACROLET. ENV is an environment as handed to &environment; its macro
and symbol-macro scope is used as the starting point."
  (let ((macros (cond ((consp env) (car env))
                      ((hash-table-p env) env)
                      (t nil)))
        (symbol-macros (and (consp env) (cdr env))))
    (%mea form
          (%mea-copy (and (hash-table-p macros) macros))
          (%mea-copy (and (hash-table-p symbol-macros) symbol-macros))
          '())))

;;; --- xref (who-calls) runtime tables ---
;;;
;;; The compiler records CALLER→CALLEE edges while compiling each named
;;; function and plants a load-time (dotcl:%xref-note caller callees) call in
;;; the compiled output. The tables live here; registration replaces the
;;; caller's whole edge set, so redefinition / fasl reload drops stale edges
;;; naturally. Keys are function names: symbols or (setf sym) lists — hence
;;; EQUAL tables (equal on interned symbols is identity).
;;; DOTCL-package names are interned at load time, not written with dotcl:
;;; reader syntax, because the SBCL cross-compile host has no DOTCL package.

(defvar *xref-callee-table* (make-hash-table :test 'equal)
  "caller name → list of callee names (compile-order).")

(defvar *xref-caller-table* (make-hash-table :test 'equal)
  "callee name → list of caller names.")

(defvar *xref-lock* (%make-lock "dotcl-xref"))

(defun %xref-note (caller callees)
  "Replace CALLER's recorded callee set with CALLEES and update the reverse
   index. Called from compiled code at load time; also usable directly."
  (%acquire-lock *xref-lock* t)
  (unwind-protect
       (progn
         ;; drop the old edges of this caller from the reverse index
         (dolist (old (gethash caller *xref-callee-table*))
           (setf (gethash old *xref-caller-table*)
                 (remove caller (gethash old *xref-caller-table*) :test #'equal)))
         (setf (gethash caller *xref-callee-table*) callees)
         (dolist (callee callees)
           (pushnew caller (gethash callee *xref-caller-table*) :test #'equal))
         caller)
    (%release-lock *xref-lock*)))

(defun %xref-who-calls (name)
  "List of function names whose definitions call (or take #' of) NAME."
  (%acquire-lock *xref-lock* t)
  (unwind-protect
       (copy-list (gethash name *xref-caller-table*))
    (%release-lock *xref-lock*)))

(defun %xref-who-is-called-by (name)
  "List of function names that NAME's definition calls (or takes #' of)."
  (%acquire-lock *xref-lock* t)
  (unwind-protect
       (copy-list (gethash name *xref-callee-table*))
    (%release-lock *xref-lock*)))

(let ((pkg (find-package "DOTCL")))
  (when pkg
    (let ((note (intern "%XREF-NOTE" pkg))
          (who  (intern "WHO-CALLS" pkg))
          (by   (intern "WHO-IS-CALLED-BY" pkg)))
      (setf (symbol-function note) #'%xref-note)
      (setf (symbol-function who) #'%xref-who-calls)
      (setf (symbol-function by) #'%xref-who-is-called-by)
      (export who pkg)
      (export by pkg))))

;;; --- Default reports for the standard condition types ---
;;;
;;; A condition signalled from Lisp with slots but no :format-control -- e.g.
;;; (error 'type-error :datum 7 :expected-type 'list), which is how much of this
;;; file reports a bad argument -- printed as "#<TYPE-ERROR>": the object holds
;;; the datum and the expected type, and said neither. Conditions raised inside
;;; the runtime carry their own message and are unaffected (they are not CLOS
;;; instances, so these methods do not apply to them).
;;;
;;; The mechanism is the one DEFINE-CONDITION's :report already uses -- a
;;; PRINT-OBJECT method that only fires when *PRINT-ESCAPE* is nil -- so a user
;;; :report on a subclass overrides these by ordinary method specificity. When
;;; the instance does carry a format control, that wins: CALL-NEXT-METHOD.

(defmacro %define-condition-report ((var class) &body body)
  "PRINT-OBJECT method for CLASS that reports via BODY when printing for a
   human and no format control was supplied."
  `(defmethod print-object ((,var ,class) stream)
     (cond (*print-escape* (call-next-method))
           ;; A simple-* subclass carries its own control string: use it, the way
           ;; the report for SIMPLE-ERROR does.
           ((simple-condition-format-control ,var)
            (apply (function format) stream
                   (simple-condition-format-control ,var)
                   (simple-condition-format-arguments ,var)))
           (t ,@body))))

(%define-condition-report (c type-error)
  (format stream "The value ~s is not of type ~s"
          (type-error-datum c) (type-error-expected-type c)))

(%define-condition-report (c unbound-variable)
  (format stream "The variable ~s is unbound." (cell-error-name c)))

(%define-condition-report (c undefined-function)
  (format stream "The function ~s is undefined." (cell-error-name c)))

(%define-condition-report (c unbound-slot)
  (format stream "The slot ~s is unbound in the object ~s."
          (cell-error-name c) (unbound-slot-instance c)))

(%define-condition-report (c cell-error)
  (format stream "The cell ~s is in error." (cell-error-name c)))

(%define-condition-report (c package-error)
  (format stream "Package error on ~a." (package-error-package c)))

(%define-condition-report (c file-error)
  (format stream "Error on file ~a." (file-error-pathname c)))

(%define-condition-report (c stream-error)
  (format stream "Stream error on ~s." (stream-error-stream c)))

(%define-condition-report (c print-not-readable)
  (format stream "The object ~a cannot be printed readably."
          (print-not-readable-object c)))
