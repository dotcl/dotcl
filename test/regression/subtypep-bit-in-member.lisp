;;; BIT is the integers 0 and 1: it is within a MEMBER that lists them, as (INTEGER 0 1) is.

(defun %sb (a b) (multiple-value-list (subtypep a b)))

(deftest subtypep-bit-member.bit
  (list (%sb 'bit '(member 0 1)) (%sb 'bit '(member 0 1 2)) (%sb 'bit '(member 0)) (%sb 'bit '(or (eql 0) (eql 1)))
        (%sb '(member 0 1) 'bit))
  ((t t) (t t) (nil t) (t t) (t t)))
