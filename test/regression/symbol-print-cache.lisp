;;; The printer remembers, on each symbol, whether its name needs escaping. The answer must follow
;;; the readtable (its syntax and its case), *print-base* and *print-readably* as they change.

(defvar cl-user::*spc-names* '("FOO" "foo" "Foo" "1+" "123" "FF" "A B" "a.b" "..." "AXB" "X" "1.5" "+5" "A:B" ""))

(defun %spc-fresh (name)
  ;; The same name on a symbol the cache has not seen, printed bare.
  (let ((*print-gensym* nil)) (prin1-to-string (make-symbol name))))

(defun %spc-check ()
  (loop for name in cl-user::*spc-names*
        for sym = (intern name)
        always (string= (let ((*print-gensym* nil)) (prin1-to-string sym))
                        (%spc-fresh name))))

(deftest symbol-print-cache.default
  (list (%spc-check) (%spc-check)) (t t))

(deftest symbol-print-cache.print-base
  (let ((s (intern "FF")) (r '()))
    (push (prin1-to-string s) r)
    (let ((*print-base* 16)) (push (prin1-to-string s) r) (push (%spc-check) r))
    (push (prin1-to-string s) r)
    (nreverse r))
  ("FF" "|FF|" t "FF"))

(deftest symbol-print-cache.readtable-case
  (let ((s (intern "foo")) (u (intern "FOO")) (r '()))
    (push (list (prin1-to-string s) (prin1-to-string u)) r)
    (let ((*readtable* (copy-readtable)))
      (setf (readtable-case *readtable*) :downcase)
      (push (list (prin1-to-string s) (prin1-to-string u) (%spc-check)) r)
      (setf (readtable-case *readtable*) :preserve)
      (push (list (prin1-to-string s) (prin1-to-string u) (%spc-check)) r))
    (push (list (prin1-to-string s) (prin1-to-string u)) r)
    (nreverse r))
  (("|foo|" "FOO") ("FOO" "|FOO|" t) ("foo" "FOO" t) ("|foo|" "FOO")))

(deftest symbol-print-cache.syntax-change-of-the-same-readtable
  (let ((s (intern "AXB")) (r '()))
    (let ((*readtable* (copy-readtable)))
      (push (prin1-to-string s) r)
      (set-syntax-from-char #\X #\Space)
      (push (prin1-to-string s) r)
      (push (%spc-check) r)
      (set-syntax-from-char #\X #\A)
      (push (prin1-to-string s) r))
    (push (prin1-to-string s) r)
    (nreverse r))
  ("AXB" "|AXB|" t "AXB" "AXB"))

(deftest symbol-print-cache.readably
  (let ((s (intern "foo")) (r '()))
    (push (prin1-to-string s) r)
    (let ((*print-readably* t)) (push (prin1-to-string s) r))
    (push (prin1-to-string s) r)
    (nreverse r))
  ("|foo|" "|foo|" "|foo|"))

(deftest symbol-print-cache.another-readtable-object
  (let ((s (intern "AXB")) (r '()) (a (copy-readtable)) (b (copy-readtable)))
    (set-syntax-from-char #\X #\Space b)
    (let ((*readtable* a)) (push (prin1-to-string s) r))
    (let ((*readtable* b)) (push (prin1-to-string s) r))
    (let ((*readtable* a)) (push (prin1-to-string s) r))
    (nreverse r))
  ("AXB" "|AXB|" "AXB"))
