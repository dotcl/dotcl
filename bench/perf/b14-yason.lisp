(load "bench/perf/pcommon.lisp")
(setf *bench-group* "14-yason")
;; yason: parse of a JSON text (2000 objects with a nested object and an array each) and encode of
;; the parsed value to a string.
;; Libraries to install on the runner: yason  (perf.yml: libs=yason)
(load-ql-system :yason)
(defun mk-json (n)
  (with-output-to-string (o)
    (write-string "[" o)
    (dotimes (i n)
      (when (plusp i) (write-string "," o))
      (format o "{\"id\":~D,\"name\":\"item ~D\",\"price\":~D.~2,'0D,\"active\":~A,\"tags\":[\"a~D\",\"b\",\"c\"],\"owner\":{\"first\":\"Ann\",\"last\":\"Lee ~D\",\"age\":~D}}"
              i i (mod i 100) (mod i 97) (if (evenp i) "true" "false") (mod i 7) i (+ 20 (mod i 50))))
    (write-string "]" o)))
(defvar *json* (mk-json 2000))
(defvar *value* (yason:parse *json*))
(defvar *sink* nil)
(bench "yason:parse, 2000 objects" 50 (setq *sink* (yason:parse *json*)))
(bench "yason:encode to a string, 2000 objects" 50
       (setq *sink* (with-output-to-string (o) (yason:encode *value* o))))
