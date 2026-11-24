;;; The variable of WITH-OUTPUT-TO-STRING is an ordinary lexical variable, as in
;;; the standard: it is not bound dynamically, so the macro costs no special
;;; binding and a closure made inside it keeps its own stream.

(deftest with-output-to-string-lexical.not-special
  (let (bound)
    (with-output-to-string (wots-s)
      (write-string "x" wots-s)
      (setq bound (boundp 'wots-s)))
    bound)
  nil)

(deftest with-output-to-string-lexical.closure-keeps-its-stream
  (let (fns)
    (dotimes (i 3)
      (with-output-to-string (wots-c)
        (push (lambda () wots-c) fns)))
    (and (every #'streamp (mapcar #'funcall fns))
         (= 3 (length (remove-duplicates (mapcar #'funcall fns))))))
  t)

(deftest with-output-to-string-lexical.nested-same-name
  (with-output-to-string (wots-n)
    (write-string "a" wots-n)
    (write-string (with-output-to-string (wots-n) (write-string "b" wots-n)) wots-n)
    (write-string "c" wots-n))
  "abc")

(deftest with-output-to-string-lexical.special-global-variable-as-stream
  (let ((r (with-output-to-string (*standard-output*) (princ "hi"))))
    r)
  "hi")

(deftest with-output-to-string-lexical.string-form
  (let ((s (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (with-output-to-string (o s) (write-string "xy" o))
    (coerce s 'simple-string))
  "xy")
