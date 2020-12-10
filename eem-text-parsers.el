(require 'parsec)

;; "internal" parsers

(defun rigpa--whitespace-parser ()
  "Whitespace parser."
  (parsec-many-as-string (parsec-re "\s")))

(defun rigpa--level-right-delimiter-parser ()
  "Parser for level right delimiter."
  (parsec-many-till-as-string (parsec-or (parsec-try (parsec-string "―")) (parsec-string "-")) ; note: not usual dash
                              (parsec-try
                               (parsec-string "|"))))

(defun rigpa--level-number-parser ()
  "Parser for level number."
  (parsec-many1-as-string (parsec-digit)))

(defun rigpa--level-left-delimiter-parser ()
  (parsec-collect-as-string
   (parsec-string "|")
   (parsec-many-till-as-string (parsec-or (parsec-try (parsec-string "―")) (parsec-string "-")) ; note: not usual dash
                               (parsec-lookahead
                                (rigpa--level-number-parser)))))

(defun rigpa--default-mode-parser ()
  "Parse default mode."
  (parsec-between (parsec-string "[")
                  (parsec-string "]")
                  (parsec-many-as-string (parsec-none-of ?\]))))

(defun rigpa--name-parser ()
  "Parse tower name."
  (parsec-between (parsec-string ":")
                  (parsec-string ":")
                  (parsec-many-as-string (parsec-none-of ?:))))

(defun rigpa--level-name-parser ()
  "Parse a level name."
  (parsec-or
   (parsec-try
    (rigpa--default-mode-parser))
   (parsec-re ".+$")))

(defun rigpa--default-level-name-parser ()
  "Parse a level name."
  (parsec-or
   (parsec-try
    (rigpa--default-mode-parser))
   nil))

(defun rigpa--level-parser ()
  "Parse a level as a list containing its number and name."
  (parsec-collect* (parsec-and (rigpa--level-left-delimiter-parser)
                               (rigpa--level-number-parser))
                   (parsec-and (rigpa--level-right-delimiter-parser)
                               (parsec-optional* (rigpa--whitespace-parser))
                               (rigpa--level-name-parser))))

(defun rigpa--level-name-only-parser ()
  "Parse the name of a level."
  (parsec-and (rigpa--level-left-delimiter-parser)
              (rigpa--level-number-parser)
              (rigpa--level-right-delimiter-parser)
              (parsec-optional* (rigpa--whitespace-parser))
              (rigpa--level-name-parser)))

(defun rigpa--level-number-only-parser ()
  "Parse the number of a level."
  (parsec-and (rigpa--level-left-delimiter-parser)
              (rigpa--level-number-parser)))

(defun rigpa--level-name-only-if-default-parser ()
  "Parse a level and extract the name if it's the default for the tower."
  (parsec-and (rigpa--level-left-delimiter-parser)
              (rigpa--level-number-parser)
              (rigpa--level-right-delimiter-parser)
              (parsec-optional* (rigpa--whitespace-parser))
              (rigpa--default-level-name-parser)))

(defun rigpa--tower-parser ()
  "Parse a string as a tower."
  (parsec-many (parsec-return (rigpa--level-parser)
                 (parsec-optional* (parsec-eol-or-eof)))))

(defun rigpa--tower-level-names-parser ()
  "Parse a string as a list of level names in a tower."
  (reverse ; the tower is parsed from top to bottom
   (parsec-many (parsec-return (rigpa--level-name-only-parser)
                  (parsec-optional* (parsec-eol-or-eof))))))

(defun rigpa--tower-default-mode-parser ()
  "Parse the default mode for a tower."
  (parsec-until (rigpa--default-mode-parser) :end))

(defun rigpa--tower-name-parser ()
  "Parse the name of the tower."
  (downcase (parsec-until (rigpa--name-parser) :end)))


;; front-ends accepting input and handling errors
(defun rigpa--parse-level-number (level)
  (let ((result (parsec-with-input level
                  (rigpa--level-number-only-parser))))
    (if (parsec-error-p result)
        (error (parsec-error-str result))
      result)))

(defun rigpa--parse-level-names (tower)
  (let ((result (parsec-with-input tower
                  (rigpa--tower-level-names-parser))))
    (if (parsec-error-p result)
        (error (parsec-error-str result))
      result)))

(defun rigpa--parse-tower-name (tower)
  (let ((result (parsec-with-input tower
                  (rigpa--tower-name-parser))))
    (if (parsec-error-p result)
        (error (parsec-error-str result))
      result)))

(defun rigpa--parse-tower-default-name (tower)
  (let ((result (parsec-with-input tower
                  (rigpa--tower-default-mode-parser))))
    (if (parsec-error-p result)
        (error (parsec-error-str result))
      result)))


(provide 'rigpa-text-parsers)
;;; rigpa-text-parsers.el ends here
