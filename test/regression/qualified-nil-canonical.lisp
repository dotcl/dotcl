;;; Reading a package-qualified NIL or T must yield the same object the bare
;;; token does. The CL package's symbol table holds ordinary Symbol entries for
;;; NIL and T, so a qualified read that returned the raw entry produced a second
;;; NIL: EQ, NULL and NOT accepted it, but a compiled test position -- which
;;; compares against the canonical false object -- saw it as true. One object,
;;; two answers to "is this false".
;;;
;;; Anything that prints a symbol with a package prefix and reads it back walks
;;; this path. Compiled files print literals fully qualified, so a literal
;;; containing NIL came back as the impostor and every IF over it took the wrong
;;; branch.

(deftest qualified-nil.read-is-false-in-test-position
  (list (if (read-from-string "nil") :truthy :falsy)
        (if (read-from-string "cl:nil") :truthy :falsy)
        (if (read-from-string "common-lisp::nil") :truthy :falsy))
  (:falsy :falsy :falsy))

(deftest qualified-nil.read-is-eq-to-nil
  (list (eq (read-from-string "cl:nil") nil)
        (eq (read-from-string "common-lisp::nil") nil)
        (null (read-from-string "cl:nil")))
  (t t t))

(deftest qualified-t.read-is-eq-to-t
  (list (eq (read-from-string "cl:t") t)
        (eq (read-from-string "common-lisp::t") t)
        (if (read-from-string "cl:t") :truthy :falsy))
  (t t :truthy))

(deftest qualified-nil.list-tail-is-a-proper-list
  (let ((l (read-from-string "(1 2 . cl:nil)")))
    (list (listp l) (length l) (mapcar #'1+ l)))
  (t 2 (2 3)))

;;; The same value after a print/read round trip under a package that forces
;;; every symbol to carry a prefix -- the shape a compiled literal takes.
(deftest qualified-nil.round-trip-through-qualified-printing
  (let* ((original (list nil t (list nil) 'cl:car))
         (repr (let ((*package* (find-package :keyword))
                     (*print-readably* t))
                 (write-to-string original)))
         (back (read-from-string repr)))
    (list (equal back original)
          (if (first back) :truthy :falsy)
          (if (second back) :truthy :falsy)
          (if (first (third back)) :truthy :falsy)))
  (t :falsy :truthy :falsy))
