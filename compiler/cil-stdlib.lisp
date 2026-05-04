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
  (cond
    ((null indices) (aref array))
    ((null (cdr indices)) (aref array (car indices)))
    ((null (cddr indices)) (aref array (car indices) (cadr indices)))
    (t (error "AREF: too many indices (>3 dimensions not supported via funcall)"))))
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
(defun - (&rest args)
  (if (null args) 0
      (if (null (cdr args))
          (- 0 (car args))
          (let ((result (car args)))
            (dolist (x (cdr args) result) (setq result (- result x)))))))
(defun * (&rest args)
  (let ((result 1))
    (dolist (x args result) (setq result (* result x)))))
(defun < (a b) (< a b))
(defun > (a b) (> a b))
(defun = (a b) (= a b))
(defun <= (a b) (<= a b))
(defun >= (a b) (>= a b))

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
  (let ((tails (cons list more-lists))
        (result nil))
    (do ((args (%map-multi tails) (%map-multi tails)))
        ((null args) result)
      (setq result (nconc result (apply function (nreverse args)))))))

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

(defun adjoin (item list &key (test #'eql) test-not (key #'identity))
  (let ((key (or key #'identity)))
    (let ((item-key (funcall key item)))
      (if (dolist (x list nil)
            (let ((k (funcall key x)))
              (when (if test-not
                        (not (funcall test-not item-key k))
                        (funcall test item-key k))
                (return t))))
          list
          (cons item list)))))

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

(defun union (list1 list2 &key (test #'eql) test-not (key #'identity))
  (let ((key (or key #'identity))
        (result (copy-list list2)))
    (dolist (x list1 result)
      (unless (%set-member (funcall key x) list2 test test-not key)
        (push x result)))))

(defun nunion (list1 list2 &key (test #'eql) test-not (key #'identity))
  (union list1 list2 :test test :test-not test-not :key key))

(defun intersection (list1 list2 &key (test #'eql) test-not (key #'identity))
  (let ((key (or key #'identity))
        (result nil))
    (dolist (x list1 (nreverse result))
      (when (%set-member (funcall key x) list2 test test-not key)
        (push x result)))))

(defun nintersection (list1 list2 &key (test #'eql) test-not (key #'identity))
  (intersection list1 list2 :test test :test-not test-not :key key))

(defun set-difference (list1 list2 &key (test #'eql) test-not (key #'identity))
  (let ((key (or key #'identity))
        (result nil))
    (dolist (x list1 (nreverse result))
      (unless (%set-member (funcall key x) list2 test test-not key)
        (push x result)))))

(defun nset-difference (list1 list2 &key (test #'eql) test-not (key #'identity))
  (set-difference list1 list2 :test test :test-not test-not :key key))

(defun set-exclusive-or (list1 list2 &key (test #'eql) test-not (key #'identity))
  ;; Elements in list1 not in list2: test called as (test (key e1) (key e2))
  ;; Elements in list2 not in list1: test also called as (test (key e1) (key e2))
  ;; so for second pass we use %set-member-rev to keep list1-key as first arg
  (let ((key (or key #'identity))
        (result nil))
    (dolist (x list1)
      (unless (%set-member (funcall key x) list2 test test-not key)
        (push x result)))
    (dolist (x list2)
      (unless (%set-member-rev (funcall key x) list1 test test-not key)
        (push x result)))
    result))

(defun nset-exclusive-or (list1 list2 &key (test #'eql) test-not (key #'identity))
  (set-exclusive-or list1 list2 :test test :test-not test-not :key key))

(defun subsetp (list1 list2 &key (test #'eql) test-not (key #'identity))
  (let ((key (or key #'identity)))
    (dolist (x list1 t)
      (unless (%set-member (funcall key x) list2 test test-not key)
        (return nil)))))

;;; ============================================================
;;; Tree operations
;;; ============================================================

(defun tree-equal (tree1 tree2 &key (test #'eql) test-not)
  (if (consp tree1)
      (and (consp tree2)
           (tree-equal (car tree1) (car tree2) :test test :test-not test-not)
           (tree-equal (cdr tree1) (cdr tree2) :test test :test-not test-not))
      (and (not (consp tree2))
           (if test-not
               (not (not (funcall test-not tree1 tree2)))
               (if (funcall test tree1 tree2) t nil)))))

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
  (subst new old tree :test test :test-not test-not :key key))

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
  (subst-if new predicate tree :key key))

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
  (subst-if-not new predicate tree :key key))

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
  ;; Convert all sequences to lists for uniform processing
  (let* ((result-type (if result-type (%typexpand-full result-type) result-type))
         (seq-lists (mapcar #'%seq-to-list sequences))
         (result nil)
         (cat (%map-rt-category result-type)))
    ;; Step through all sequences in parallel
    (block outer
      (loop
        (let ((args nil)
              (done nil))
          (dolist (lst seq-lists)
            (when (null lst) (setq done t)))
          (when done (return-from outer))
          (dolist (lst seq-lists)
            (push (car lst) args))
          (push (apply function (nreverse args)) result)
          (setq seq-lists (mapcar #'cdr seq-lists)))))
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
       ;; Check compound size constraint: (vector * n)
       (when (and (consp result-type) (consp (cdr result-type)) (consp (cddr result-type))
                  (integerp (caddr result-type))
                  (/= (length result) (caddr result-type)))
         (error 'type-error :datum result :expected-type result-type))
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

(defun rationalize (x) (rational x))

(defun conjugate (z)
  (if (complexp z)
      (complex (realpart z) (- (imagpart z)))
      z))

;;; ===== DOCUMENTATION generic function (CLHS) =====

;; Global storage: (cons object doc-type) → string
(defvar *%pprint-level* 0)

(defvar *compilation-unit-depth* 0)
(defvar *deferred-compilation-warnings* nil)

(defvar *documentation-table* (make-hash-table :test #'equal))

;; Helper to make a key for the documentation table
(defun %doc-key (obj doc-type)
  (cons obj doc-type))

;; Helper to get documentation (returns single value, not gethash's two values)
(defun %get-doc (obj doc-type)
  (values (gethash (%doc-key obj doc-type) *documentation-table*)))

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

;; (documentation <symbol> 'function) — via symbol-function
(defmethod documentation ((x symbol) (doc-type (eql 'function)))
  (%get-doc x 'function))

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

;;; --- ensure-generic-function ---
(defun ensure-generic-function (name &rest args)
  ;; Odd number of keyword args → program-error
  (when (oddp (length args))
    (error 'program-error))
  (let ((lambda-list-p nil)
        (lambda-list nil))
    ;; Manual keyword parsing
    (do ((rest args (cddr rest)))
        ((null rest))
      (when (eq (car rest) :lambda-list)
        (setf lambda-list (cadr rest))
        (setf lambda-list-p t)))
    ;; If already fbound
    (if (fboundp name)
        (let ((fn (fdefinition name)))
          (if (typep fn 'generic-function)
              fn
              (error 'program-error)))
        ;; Create new GF
        (let* ((ll (if lambda-list-p lambda-list '()))
               (arity (length (remove-if (lambda (x)
                                (member x '(&rest &optional &key &allow-other-keys &body &aux)))
                              ll)))
               (gf (%make-gf name arity)))
          (%register-gf name gf)
          (setf (fdefinition name) gf)
          gf))))

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

