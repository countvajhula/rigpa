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

(defun eem--parse-default-mode ()
  "Parse default mode."
  (parsec-between (parsec-string "[")
                  (parsec-string "]")
                  (parsec-many-as-string (parsec-none-of ?\]))))

(defun eem--parse-name ()
  "Parse tower name."
  (parsec-between (parsec-string "-")
                  (parsec-string "-")
                  (parsec-many-as-string (parsec-none-of ?-))))

(defun eem--parse-level-name ()
  "Parse a level name."
  (parsec-or
   (parsec-try
    (eem--parse-default-mode))
   (parsec-re ".+$")))

(defun eem--parse-default-level-name ()
  "Parse a level name."
  (parsec-or
   (parsec-try
    (eem--parse-default-mode))
   nil))

(defun eem--parse-level ()
  "Parse a level as a list containing its number and name."
  (parsec-collect* (parsec-and (eem--parse-level-left-delimiter)
                               (eem--parse-level-number))
                   (parsec-and (eem--parse-level-right-delimiter)
                               (parsec-optional* (eem--parse-whitespace))
                               (eem--parse-level-name))))

(defun eem--parse-level-name-only ()
  "Parse a level as a list containing its number and name."
  (parsec-and (eem--parse-level-left-delimiter)
              (eem--parse-level-number)
              (eem--parse-level-right-delimiter)
              (parsec-optional* (eem--parse-whitespace))
              (eem--parse-level-name)))

(defun eem--parse-level-name-only-if-default ()
  "Parse a level and extract the name if it's the default for the tower."
  (parsec-and (eem--parse-level-left-delimiter)
              (eem--parse-level-number)
              (eem--parse-level-right-delimiter)
              (parsec-optional* (eem--parse-whitespace))
              (eem--parse-default-level-name)))

(defun eem--parse-tower ()
  "Parse a string as a tower."
  (parsec-many (parsec-return (eem--parse-level)
                 (parsec-optional* (parsec-eol-or-eof)))))

(defun eem--parse-tower-level-names ()
  "Parse a string as a list of level names in a tower."
  (reverse ; the tower is parsed from top to bottom
   (parsec-many (parsec-return (eem--parse-level-name-only)
                  (parsec-optional* (parsec-eol-or-eof))))))

(defun eem--parse-tower-default-mode ()
  "Parse the default mode for a tower."
  (parsec-until (eem--parse-default-mode) :end))

(defun eem--parse-tower-name ()
  "Parse the name of the tower."
  (downcase (parsec-until (eem--parse-name) :end)))

(provide 'eem-text-parsers)
;;; eem-text-parsers.el ends here
