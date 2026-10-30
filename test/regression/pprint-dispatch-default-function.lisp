;;; PPRINT-DISPATCH returns a default function with a false secondary value
;;; when no entry matches (CLHS PPRINT-DISPATCH).

(deftest pprint-dispatch-default-function.unmatched
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
        (*print-pretty* t))
    (multiple-value-bind (function found)
        (pprint-dispatch 42)
      (list (not (null function)) found)))
  #.(list t nil))
