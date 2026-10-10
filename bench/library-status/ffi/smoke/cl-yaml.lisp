;; cl-libyaml allocates yaml_parser_t from its CFFI struct definition, whose
;; size_t fields are :unsigned-long on 64-bit hosts.
(smoke "parser-t size" (cffi:foreign-type-size '(:struct libyaml.parser::parser-t)))
(let ((h (yaml:parse "a: [1, 2.5, x]")))
  (smoke "parse" (gethash "a" h)))
(smoke "emit" (yaml:emit-to-string (list 1 "two" 3.5)))
