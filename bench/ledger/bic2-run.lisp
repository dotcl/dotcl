;;;; Compiles bic2.lisp to the fasl named by LEDGER_FASL and loads it; the file
;;;; prints "BIC2 <ns per pixel> ns/px chk <checksum>".
(require "asdf")
(load (compile-file (merge-pathnames "bic2.lisp" (or *load-truename* *default-pathname-defaults*))
                    :output-file (uiop:getenv "LEDGER_FASL")))
#+dotcl (dotcl:quit 0)
#+sbcl (sb-ext:exit :code 0)
