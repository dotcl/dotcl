;;; DOTCL:TYPEXPAND-1 / TYPEXPAND expand the built-in types that SBCL defines
;;; as abbreviations of others, with SB-EXT:TYPEXPAND-1's answers: (MOD n),
;;; (UNSIGNED-BYTE n) and (SIGNED-BYTE n) as INTEGER ranges, BOOLEAN as
;;; (MEMBER T NIL). They were returned unexpanded. type-i (through
;;; introspect-environment's TYPEXPAND-1) infers (INTEGER 0 4) from (MOD 5).
;;; Expected values are SBCL 2.6.8's answers.

(deftest typexpand-builtin-abbreviations.expanded
  (mapcar (lambda (ty) (multiple-value-list (dotcl:typexpand-1 ty)))
          '((mod 5) (mod 1) (unsigned-byte 8) (unsigned-byte 1) (unsigned-byte *) unsigned-byte
            (signed-byte 8) (signed-byte 1) (signed-byte *) signed-byte boolean (boolean)))
  (((integer 0 4) t) ((integer 0 0) t) ((integer 0 255) t) ((integer 0 1) t) ((integer 0) t)
   ((integer 0) t) ((integer -128 127) t) ((integer -1 0) t) (integer t) (integer t)
   ((member t nil) t) ((member t nil) t)))

(deftest typexpand-builtin-abbreviations.unchanged
  (mapcar (lambda (ty) (multiple-value-list (dotcl:typexpand-1 ty)))
          '(fixnum bit (bit) keyword (member 1 2) (integer 0 3) short-float (string 3)))
  ((fixnum nil) (bit nil) ((bit) nil) (keyword nil) ((member 1 2) nil) ((integer 0 3) nil)
   (short-float nil) ((string 3) nil)))

(deftest typexpand-builtin-abbreviations.typexpand
  (list (multiple-value-list (dotcl:typexpand '(mod 5)))
        (multiple-value-list (dotcl:typexpand '(unsigned-byte 64))))
  (((integer 0 4) t) ((integer 0 18446744073709551615) t)))
