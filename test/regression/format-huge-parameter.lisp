;;; A FORMAT parameter so large that the output cannot be sized is a STORAGE-CONDITION with a
;;; FORMAT message, not a raw arithmetic-overflow text.

(deftest format-huge-parameter.storage-condition
  (list (handler-case (format nil "~999999999999f" 1/3) (storage-condition () :storage-condition))
        (handler-case (format nil "~99999999999a" 1) (storage-condition () :storage-condition))
        (handler-case (funcall (formatter "~999999999999f") nil 1/3) (storage-condition () :storage-condition)))
  (:storage-condition :storage-condition :storage-condition))

(deftest format-huge-parameter.message
  (let ((m (handler-case (format nil "~999999999999f" 1/3) (storage-condition (c) (princ-to-string c)))))
    (and (search "FORMAT" m) (not (search "Arithmetic operation" m)) t))
  t)
