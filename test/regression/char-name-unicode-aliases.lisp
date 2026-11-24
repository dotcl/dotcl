;;; NAME-CHAR and the reader accept the Unicode name aliases of the control characters, as SBCL does
;;; (values from SBCL 2.6.8). CHAR-NAME is unchanged.
(deftest char-name-unicode-aliases.name-char
  (mapcar (lambda (n) (let ((c (name-char n))) (and c (char-code c))))
          '("FormFeed" "FORMFEED" "formfeed" "Nl" "NL" "Start_Of_Heading" "START_OF_TEXT" "End_Of_Text"
            "Acknowledge" "Cancel" "Substitute" "Shift_Out" "Shift_In" "Data_Link_Escape"
            "Device_Control_One" "Device_Control_Four" "Negative_Acknowledge" "Synchronous_Idle"
            "Information_Separator_Four" "Information_Separator_One" "Character_Tabulation"
            "Line_Tabulation" "Start_Of_String" "String_Terminator" "Byte_Order_Mark"))
  (12 12 12 10 10 1 2 3 6 24 26 14 15 16 17 20 21 22 28 31 9 11 152 156 65279))

(deftest char-name-unicode-aliases.reader
  (list (char-code #\FormFeed) (char-code #\Nl) (char-code #\Start_Of_Heading) (char-code #\End_Of_Medium))
  (12 10 1 25))

(deftest char-name-unicode-aliases.char-name-unchanged
  (list (char-name (code-char 12)) (char-name (code-char 10)) (char-name (code-char 1)))
  ("Page" "Newline" "Soh"))
