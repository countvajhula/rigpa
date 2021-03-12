(require 'chimera)
(require 'chimera-hydra)
(require 'rigpa-evil-support)

(defvar rigpa-word-mode-map (make-sparse-keymap))

(define-minor-mode rigpa-word-mode
  "Minor mode to modulate keybindings in rigpa word mode."
  :lighter "word"
  :keymap rigpa-word-mode-map)

(evil-define-state word
  "Word state."
  :tag " <W> "
  :message "-- WORD --"
  :enable (normal))

(evil-define-motion rigpa-word-backward (count)
  "Motion for moving backward by a word."
  :type exclusive
  (let ((count (or count 1)))
    (evil-backward-WORD-begin count)))

(evil-define-motion rigpa-word-forward (count)
  "Motion for moving forward by a word."
  :type exclusive
  (let ((count (or count 1)))
    (evil-forward-WORD-begin count)))

(defun rigpa-word--select-word ()
  "Select nearest word, going backwards if necessary."
  (let ((on-word-p (save-excursion
                       (let ((original-position (point)))
                         (evil-backward-WORD-begin)
                         (evil-forward-WORD-begin)
                         (= (point) original-position)))))
      (unless on-word-p
        (let ((original-line (line-number-at-pos)))
          (cond ((save-excursion (evil-backward-WORD-begin)
                                 (= (line-number-at-pos)
                                    original-line))
                 (evil-backward-WORD-begin))
                ((save-excursion (evil-forward-WORD-begin)
                                 (= (line-number-at-pos)
                                    original-line))
                 (evil-forward-WORD-begin)))))))

(evil-define-motion rigpa-word-up (count)
  "Motion for moving up by a word."
  :type exclusive
  (let ((count (or count 1)))
    (evil-previous-line count)
    (rigpa-word--select-word)))

(evil-define-motion rigpa-word-down (count)
  "Motion for moving down by a word."
  :type exclusive
  (let ((count (or count 1)))
    (evil-next-line count)
    (rigpa-word--select-word)))

(evil-define-operator rigpa-word-delete (beg end type register yank-handler)
  "Delete word."
  :motion rigpa-word-forward
  (evil-delete beg end type register yank-handler))

(evil-define-operator rigpa-word-change (beg end type register yank-handler)
  "Change word."
  :motion rigpa-word-forward
  (evil-change beg end type register yank-handler))

(evil-define-operator rigpa-word-toggle-case (beg end type register yank-handler)
  "Toggle case."
  :motion rigpa-word-forward
  (evil-invert-case beg end))

(evil-define-operator rigpa-word-upper-case (beg end type register yank-handler)
  "Make upper case."
  :motion rigpa-word-forward
  (evil-upcase beg end))

(evil-define-operator rigpa-word-lower-case (beg end type register yank-handler)
  "Make lower case."
  :motion rigpa-word-forward
  (evil-downcase beg end))

(evil-define-operator rigpa-word-title-case (beg end type register yank-handler)
  "Make title case."
  :motion rigpa-word-forward
  (capitalize-region beg end))

(evil-define-command rigpa-word-move-backward (count)
  "Move word backwards."
  (interactive "p")
  (dotimes (i count)
    (evil-inner-word)
    (evil-backward-WORD-begin)
    (transpose-words 1)
    (evil-backward-WORD-begin 2)))

(evil-define-command rigpa-word-move-forward (count)
  "Move word forward"
  (interactive "p")
  (dotimes (i count)
    (evil-forward-WORD-begin)
    (transpose-words 1)
    (evil-backward-WORD-begin)))

(evil-define-command rigpa-word-move-down (count)
  "Move word down"
  (interactive "p")
  (dotimes (i count)
    (evil-inner-word nil nil nil nil)
    (setq word-end-position (point))
    (evil-backward-WORD-begin nil)
    (evil-delete (point) word-end-position 'exclusive nil nil)
    (evil-next-line)
    (evil-paste-before nil nil)))

(evil-define-command rigpa-word-move-up (count)
  "Move word up"
  (interactive "p")
  (dotimes (i count)
    (evil-inner-word nil nil nil nil)
    (setq word-end-position (point))
    (evil-backward-WORD-begin nil)
    (evil-delete (point) word-end-position 'exclusive nil nil)
    (evil-previous-line)
    (evil-paste-before nil nil)))

(defun rigpa-word-split ()
  "Split word into characters on separate lines."
  (interactive)
  (with-undo-collapse
    (apply #'rigpa-word-delete (evil-inner-WORD))
    (evil-open-below 1)
    (evil-force-normal-state)
    (evil-paste-after nil nil)
    (evil-beginning-of-line)
    (while (not (eolp))
      (evil-forward-char)
      (newline)
      (evil-force-normal-state))))

(defun rigpa-word-delete-others ()
  "Delete other words in line."
  (interactive)
  (with-undo-collapse
    (apply #'rigpa-word-delete (evil-inner-WORD))
    (evil-open-below 1)
    (evil-force-normal-state)
    (evil-paste-after nil nil)
    (evil-previous-line)
    (rigpa-line-delete)))

(defun rigpa-word-rotate-chars-right ()
  "Rotate characters to the right"
  (interactive)
  (save-excursion
    (let* ((word-bounds (evil-inner-word))
           (word-start (nth 0 word-bounds))
           (word-end (nth 1 word-bounds)))
      (evil-delete-backward-char (- word-end 1)
                                 word-end
                                 'exclusive
                                 nil)
      (goto-char word-start)
      (evil-paste-before nil nil))))

(defun rigpa-word-rotate-chars-left ()
  "Rotate characters to the left"
  (interactive)
  (save-excursion
    (let* ((word-bounds (evil-inner-word))
           (word-start (nth 0 word-bounds))
           (word-end (nth 1 word-bounds)))
      (evil-delete-char word-start
                        (+ word-start 1)
                        'exclusive
                        nil)
      (goto-char (- word-end 1))
      (evil-paste-before nil nil))))

(defun rigpa-word-scroll-jump-backward ()
  "Scroll jump back across words."
  (interactive)
  (evil-backward-WORD-begin 3))

(defun rigpa-word-scroll-jump-forward ()
  "Scroll jump forward across words."
  (interactive)
  (evil-forward-WORD-begin 3))

(defun rigpa-word-first-word ()
  "Jump backward to the first word in the paragraph."
  (interactive)
  (evil-backward-paragraph)
  (evil-forward-WORD-begin))

(defun rigpa-word-last-word ()
  "Jump forward to the last word in the paragraph."
  (interactive)
  (evil-forward-paragraph)
  (evil-backward-WORD-begin))

(defun rigpa-word-add-to-end ()
  "Add to the end of this word."
  (interactive)
  (evil-forward-WORD-end)
  (forward-char)
  (evil-insert-state))

(defun rigpa-word-add-after ()
  "Add a word after this one."
  (interactive)
  (evil-forward-WORD-begin)
  (insert " ")
  (backward-char)
  (evil-insert-state))

(defun rigpa-word-add-before ()
  "Add a word before this one."
  (interactive)
  (evil-backward-WORD-end)
  (forward-char)
  (insert " ")
  (evil-insert-state))

(defun rigpa-word-add-to-beginning ()
  "Add to the beginning of this word."
  (interactive)
  (evil-insert-state))

(defvar rigpa--word-mode-keyspec
  '(("h" . rigpa-word-backward)
    ("j" . rigpa-word-down)
    ("k" . rigpa-word-up)
    ("l" . rigpa-word-forward)
    ("H" . rigpa-word-move-backward)
    ("J" . rigpa-word-move-down)
    ("K" . rigpa-word-move-up)
    ("L" . rigpa-word-move-forward)
    ("x" . rigpa-word-delete)
    ("c" . rigpa-word-change)
    ("~" . rigpa-word-toggle-case)
    ("gU" . rigpa-word-upper-case)
    ("gu" . rigpa-word-lower-case)
    ("gt" . rigpa-word-title-case)
    ("s" . rigpa-word-split)
    ("s-o" . rigpa-word-delete-others)
    ("C-S-h" . rigpa-word-rotate-chars-left)
    ("C-S-l" . rigpa-word-rotate-chars-right)
    ("C-h" . rigpa-word-scroll-jump-backward)
    ("C-k" . rigpa-word-scroll-jump-backward)
    ("C-j" . rigpa-word-scroll-jump-forward)
    ("C-l" . rigpa-word-scroll-jump-forward)
    ("M-h" . rigpa-word-first-word)
    ("0" . rigpa-word-first-word)
    ("M-l" . rigpa-word-last-word)
    ("$" . rigpa-word-last-word)
    ("a" . rigpa-word-add-to-end)
    ("i" . rigpa-word-add-to-beginning)
    ("A" . rigpa-word-add-after)
    ("I" . rigpa-word-add-before)
    ("?" . dictionary-lookup-definition))
  "Key specification for rigpa word mode.")

;; TODO: review these:
;; exiting keys: c, a, i, A, I, s-r (delete), s-o (delete others), ?, Esc, Ret

;; TODO: review remaining defuns for possible conversion
;; to evil operators, motions, and commands

;; TODO: review accuracy and behavior

(rigpa--define-evil-keys-from-spec rigpa--word-mode-keyspec
                                   rigpa-word-mode-map)

(defvar chimera-word-mode-entry-hook nil
  "Entry hook for rigpa word mode.")

(defvar chimera-word-mode-exit-hook nil
  "Exit hook for rigpa word mode.")

(defun rigpa--enable-word-minor-mode ()
  "Enable word minor mode."
  (rigpa-word-mode 1))

(defun rigpa--disable-word-minor-mode ()
  "Disable word minor mode."
  (rigpa-word-mode -1))


(defvar chimera-word-mode
  (make-chimera-mode :name "word"
                     :enter #'evil-word-state
                     :pre-entry-hook 'chimera-word-mode-entry-hook
                     :post-exit-hook 'chimera-word-mode-exit-hook
                     :entry-hook 'evil-word-state-entry-hook
                     :exit-hook 'evil-word-state-exit-hook))


(provide 'rigpa-word-mode)
