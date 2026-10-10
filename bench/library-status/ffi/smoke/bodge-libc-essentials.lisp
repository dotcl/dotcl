(cffi:with-foreign-strings ((a "abc") (b "abd"))
  (smoke "strcmp" (signum (funcall (find-symbol "STRCMP" "%LIBC.ES") a b))))
