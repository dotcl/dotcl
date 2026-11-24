(load "bench/perf/pcommon.lisp")
(setf *bench-group* "15-cxml")
;; cxml: parse of an XML text of 3000 items (about 180 KB) with the default SAX handler and with the DOM builder.
;; Libraries to install on the runner: cxml  (perf.yml: libs=cxml)
(load-ql-system :cxml)
(defun mk-xml (n)
  (with-output-to-string (o)
    (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?><catalog>" o)
    (dotimes (i n)
      (format o "<item id=\"~D\" kind=\"k~D\"><title>Item number ~D</title><desc>Some text &amp; more text for item ~D, long enough to matter.</desc><price cur=\"EUR\">~D.99</price></item>"
              i (mod i 5) i i (mod i 100)))
    (write-string "</catalog>" o)))
(defvar *xml* (mk-xml 3000))
(defvar *sink* nil)
(bench "cxml:parse, default sax handler, 3000 items" 50
       (setq *sink* (cxml:parse *xml* (make-instance 'sax:default-handler))))
(bench "cxml:parse, dom builder, 3000 items" 50
       (setq *sink* (cxml:parse *xml* (cxml-dom:make-dom-builder))))
