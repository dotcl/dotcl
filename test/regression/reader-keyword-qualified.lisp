;;; KEYWORD:NAME reads as :NAME even when the symbol does not exist yet
;;; (every symbol of the KEYWORD package is external).

(deftest reader-keyword-qualified.new-symbol
  (let ((r (read-from-string "keyword:rdkq-fresh-name-1")))
    (list (keywordp r) (eq r :rdkq-fresh-name-1) (symbol-name r)))
  (t t "RDKQ-FRESH-NAME-1"))

(deftest reader-keyword-qualified.double-colon
  (eq (read-from-string "keyword::rdkq-fresh-name-2") :rdkq-fresh-name-2)
  t)
