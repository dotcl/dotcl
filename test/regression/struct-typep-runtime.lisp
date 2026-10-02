;;; TYPEP of a structure instance against a symbol that is only known at run
;;; time (the compiler cannot open-code it), which goes through the runtime's
;;; TYPEP. A symbol naming a structure class is answered from the instance's
;;; class precedence list directly; these pin that the answers match the
;;; general path: the own type, an included ancestor, a descendant, an
;;; unrelated structure, T / STRUCTURE-OBJECT / ATOM, and types that are not
;;; structures. (A same-named symbol from another package keeps the general
;;; path, which matches structure names across packages for older fasls.)

(defstruct strt-a x)
(defstruct (strt-b (:include strt-a)) y)
(defstruct (strt-c (:include strt-b)) z)
(defstruct strt-other w)

(defun strt-typep (obj type) (typep obj type))

(deftest struct-typep-runtime.hierarchy
  (let ((b (make-strt-b)))
    (mapcar (lambda (ty) (strt-typep b ty))
            '(strt-b strt-a strt-c strt-other)))
  (t t nil nil))

(deftest struct-typep-runtime.general-types
  (let ((c (make-strt-c)))
    (mapcar (lambda (ty) (and (strt-typep c ty) t))
            '(t structure-object atom strt-a list number)))
  (t t t t nil nil))
