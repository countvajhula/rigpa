(require 'parsec)

(defun eem--parse-whitespace ()
  "Whitespace parser."
  (parsec-many-as-string (parsec-re "\s")))

(defun eem--parse-level-right-delimiter ()
  "Parser for level right delimiter."
  (parsec-many-till-as-string (parsec-string "―")
                              (parsec-try
                               (parsec-string "|"))))

(defun eem--parse-level-number ()
  "Parser for level number."
  (parsec-re "[0-9]+"))

(defun eem--parse-level-left-delimiter ()
  (parsec-and (parsec-string "|")
              (parsec-many-till-as-string (parsec-string "―")
                                          (parsec-lookahead
                                           (eem--parse-level-number)))))

(defun eem--parse-level ()
  (parsec-collect* (parsec-and (eem--parse-level-left-delimiter)
                               (eem--parse-level-number))
                   (parsec-and (eem--parse-level-right-delimiter)
                               (parsec-optional* (eem--parse-whitespace))
                               (parsec-re ".*$"))))

(defun eem--parse-tower ()
  "Parse a string as a tower."
  (parsec-many (parsec-return (eem--parse-level)
                 (parsec-optional* (parsec-or (parsec-ch ?\n)
                                              (parsec-eof))))))

(provide 'eem-text-parsers)
;;; eem-text-parsers.el ends here
