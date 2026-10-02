;;; Regression: #\Slash, #\Backslash and #\Period were "Unknown character
;;; name". SBCL reads them (they are older names of SOLIDUS, REVERSE SOLIDUS
;;; and FULL STOP); charje.asdf-cli writes #\slash. They are aliases for
;;; reading only: CHAR-NAME keeps returning the canonical names.

(deftest char-name-sbcl-aliases-read
  (list (name-char "slash") (name-char "Backslash") (name-char "PERIOD")
        (read-from-string "#\\slash") (read-from-string "#\\period"))
  (#\/ #\\ #\. #\/ #\.))

(deftest char-name-sbcl-aliases-not-printed
  (list (char-name #\/) (char-name #\\) (char-name #\.))
  ("Solidus" "Reverse_solidus" "Full_stop"))
