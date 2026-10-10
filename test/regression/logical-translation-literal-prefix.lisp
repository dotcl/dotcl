;;; A translation rule whose from-wildcard names directories before its **:
;;; those directories are what the rule matched on, not part of what ** stands
;;; for. ("FOO:SRC;**;*.*.*" "/x/**/*.*") takes FOO:SRC;A;B.LISP to /x/a/b.lisp.
;;; TRANSLATE-LOGICAL-PATHNAME used to copy the whole logical directory into the
;;; place of ** and answered /x/src/a/b.lisp. Expected values are SBCL 2.6.8's.

(setf (logical-pathname-translations "LTLPFOO")
      '(("LTLPFOO:SRC;**;*.*.*" "/tmp/ltlp-x/**/*.*")
        ("LTLPFOO:OTHER;DEEP;**;*.*.*" "/tmp/ltlp-y/**/*.*")
        ("LTLPFOO:**;*.*.*" "/tmp/ltlp-z/**/*.*")))

(deftest logical-translation-literal-prefix.one
  (namestring (translate-logical-pathname "LTLPFOO:SRC;A;B.LISP"))
  "/tmp/ltlp-x/a/b.lisp")

(deftest logical-translation-literal-prefix.two
  (namestring (translate-logical-pathname "LTLPFOO:OTHER;DEEP;C.LISP"))
  "/tmp/ltlp-y/c.lisp")

(deftest logical-translation-literal-prefix.none
  (namestring (translate-logical-pathname "LTLPFOO:Q;R.LISP"))
  "/tmp/ltlp-z/q/r.lisp")
