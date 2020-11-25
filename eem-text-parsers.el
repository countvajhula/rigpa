(require 'parsec)

;; "internal" parsers

(defun eem--whitespace-parser ()
  "Whitespace parser."
  (parsec-many-as-string (parsec-re "\s")))

(defun eem--level-right-delimiter-parser ()
  "Parser for level right delimiter."
  (parsec-many-till-as-string (parsec-string "―") ; note: not usual dash
                              (parsec-try
                               (parsec-string "|"))))

(defun eem--level-number-parser ()
  "Parser for level number."
  (parsec-many1-as-string (parsec-digit)))

(defun eem--level-left-delimiter-parser ()
  (parsec-collect-as-string
   (parsec-string "|")
   (parsec-many-till-as-string (parsec-string "―") ; note: not usual dash
                               (parsec-lookahead
                                (eem--level-number-parser)))))

(defun eem--default-mode-parser ()
  "Parse default mode."
  (parsec-between (parsec-string "[")
                  (parsec-string "]")
                  (parsec-many-as-string (parsec-none-of ?\]))))

(defun eem--name-parser ()
  "Parse tower name."
  (parsec-between (parsec-string "-")
                  (parsec-string "-")
                  (parsec-many-as-string (parsec-none-of ?-))))

(defun eem--level-name-parser ()
  "Parse a level name."
  (parsec-or
   (parsec-try
    (eem--default-mode-parser))
   (parsec-re ".+$")))

(defun eem--default-level-name-parser ()
  "Parse a level name."
  (parsec-or
   (parsec-try
    (eem--default-mode-parser))
   nil))

(defun eem--level-parser ()
  "Parse a level as a list containing its number and name."
  (parsec-collect* (parsec-and (eem--level-left-delimiter-parser)
                               (eem--level-number-parser))
                   (parsec-and (eem--level-right-delimiter-parser)
                               (parsec-optional* (eem--whitespace-parser))
                               (eem--level-name-parser))))

(defun eem--level-name-only-parser ()
  "Parse the name of a level."
  (parsec-and (eem--level-left-delimiter-parser)
              (eem--level-number-parser)
              (eem--level-right-delimiter-parser)
              (parsec-optional* (eem--whitespace-parser))
              (eem--level-name-parser)))

(defun eem--level-number-only-parser ()
  "Parse the number of a level."
  (parsec-and (eem--level-left-delimiter-parser)
              (eem--level-number-parser)))

(defun eem--level-name-only-if-default-parser ()
  "Parse a level and extract the name if it's the default for the tower."
  (parsec-and (eem--level-left-delimiter-parser)
              (eem--level-number-parser)
              (eem--level-right-delimiter-parser)
              (parsec-optional* (eem--whitespace-parser))
              (eem--default-level-name-parser)))

(defun eem--tower-parser ()
  "Parse a string as a tower."
  (parsec-many (parsec-return (eem--level-parser)
                 (parsec-optional* (parsec-eol-or-eof)))))

(defun eem--tower-level-names-parser ()
  "Parse a string as a list of level names in a tower."
  (reverse ; the tower is parsed from top to bottom
   (parsec-many (parsec-return (eem--level-name-only-parser)
                  (parsec-optional* (parsec-eol-or-eof))))))

(defun eem--tower-default-mode-parser ()
  "Parse the default mode for a tower."
  (parsec-until (eem--default-mode-parser) :end))

(defun eem--tower-name-parser ()
  "Parse the name of the tower."
  (downcase (parsec-until (eem--name-parser) :end)))


;; front-ends accepting input and handling errors
(defun eem--parse-level-number (level)
  (let ((result (parsec-with-input level
                  (eem--level-number-only-parser))))
    (if (parsec-error-p result)
        (error (parsec-error-str result))
      result)))

(defun eem--parse-level-names (tower)
  (let ((result (parsec-with-input tower
                  (eem--tower-level-names-parser))))
    (if (parsec-error-p result)
        (error (parsec-error-str result))
      result)))

(defun eem--parse-tower-name (tower)
  (let ((result (parsec-with-input tower
                  (eem--tower-name-parser))))
    (if (parsec-error-p result)
        (error (parsec-error-str result))
      result)))

(defun eem--parse-tower-default-name (tower)
  (let ((result (parsec-with-input tower
                  (eem--tower-default-mode-parser))))
    (if (parsec-error-p result)
        (error (parsec-error-str result))
      result)))


(provide 'eem-text-parsers)
;;; eem-text-parsers.el ends here
