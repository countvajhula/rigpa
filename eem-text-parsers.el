(require 'parsec)

(defun eem--parse-whitespace ()
  "Whitespace parser."
  (parsec-many-as-string (parsec-re "\s")))

(defun eem--parse-level-right-delimiter ()
  "Parser for level right delimiter."
  (parsec-many-till-as-string (parsec-string "―") ; note: not usual dash
                              (parsec-try
                               (parsec-string "|"))))

(defun eem--parse-level-number ()
  "Parser for level number."
  (parsec-many1-as-string (parsec-digit)))

(defun eem--parse-level-left-delimiter ()
  (parsec-and (parsec-string "|")
              (parsec-many-till-as-string (parsec-string "―") ; note: not usual dash
                                          (parsec-lookahead
                                           (eem--parse-level-number)))))

(defun eem--parse-level ()
  "Parse a level as a list containing its number and name."
  (parsec-collect* (parsec-and (eem--parse-level-left-delimiter)
                               (eem--parse-level-number))
                   (parsec-and (eem--parse-level-right-delimiter)
                               (parsec-optional* (eem--parse-whitespace))
                               (parsec-re ".*$"))))

(defun eem--parse-level-name-only ()
  "Parse a level as a list containing its number and name."
  (parsec-and (eem--parse-level-left-delimiter)
              (eem--parse-level-number)
              (eem--parse-level-right-delimiter)
              (parsec-optional* (eem--parse-whitespace))
              (parsec-re ".*$")))

(defun eem--parse-tower ()
  "Parse a string as a tower."
  (parsec-many (parsec-return (eem--parse-level)
                 (parsec-optional* (parsec-eol-or-eof)))))

(defun eem--parse-tower-level-names ()
  "Parse a string as a list of level names in a tower."
  (reverse ; the tower is parsed from top to bottom
   (parsec-many (parsec-return (eem--parse-level-name-only)
                  (parsec-optional* (parsec-eol-or-eof))))))

(provide 'eem-text-parsers)
;;; eem-text-parsers.el ends here
