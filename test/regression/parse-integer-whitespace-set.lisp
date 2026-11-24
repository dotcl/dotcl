;;; PARSE-INTEGER skips the standard whitespace characters (space, tab, newline, return, page) around
;;; the number, and nothing else: a vertical tab, a no-break space or a Unicode space is junk.

(defun %pi (prefix suffix &optional junk)
  (handler-case (parse-integer (format nil "~A12~A" prefix suffix) :junk-allowed junk)
    (error () :error)))

(deftest parse-integer-whitespace-set.standard
  (list (%pi #\Space #\Space) (%pi #\Tab #\Newline) (%pi #\Return #\Page) (%pi "" ""))
  (12 12 12 12))

(deftest parse-integer-whitespace-set.not-whitespace
  (list (%pi (code-char 11) "") (%pi (code-char 160) "") (%pi (code-char #x2003) "")
        (%pi "" (code-char 160)) (%pi (code-char 133) "")
        (%pi (code-char 160) "" t) (%pi "" (code-char 160) t))
  (:error :error :error :error :error nil 12))
