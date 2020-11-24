(require 'chimera)
(require 'chimera-hydra)

(evil-define-state word
  "Word state."
  :tag " <W> "
  :message "-- WORD --"
  :enable (normal))

(defun my-move-word-backward ()
  "Move word backwards"
  (interactive)
  (evil-inner-word nil nil nil nil)
  (evil-backward-WORD-begin nil)
  (transpose-words 1)
  (evil-backward-WORD-begin 2))

(defun my-move-word-forward ()
  "Move word forward"
  (interactive)
  (evil-forward-WORD-begin nil)
  (transpose-words 1))

(defun my-move-word-down ()
  "Move word down"
  (interactive)
  (evil-inner-word nil nil nil nil)
  (setq word-end-position (point))
  (evil-backward-WORD-begin nil)
  (evil-delete (point) word-end-position 'exclusive nil nil)
  (evil-next-line)
  (evil-paste-before nil nil))

(defun my-move-word-up ()
  "Move word up"
  (interactive)
  (evil-inner-word nil nil nil nil)
  (setq word-end-position (point))
  (evil-backward-WORD-begin nil)
  (evil-delete (point) word-end-position 'exclusive nil nil)
  (evil-previous-line)
  (evil-paste-before nil nil))

(defun my-delete-word ()
  "Delete word"
  (interactive)
  (apply 'evil-delete (evil-inner-word)))

(defun my-change-word ()
  "Change word"
  (interactive)
  (apply 'evil-change (evil-inner-word)))

(defun my-toggle-case ()
  "Toggle case"
  (interactive)
  (save-excursion
    (apply 'evil-invert-case (evil-inner-word))))

(defun my-upper-case ()
  "Make upper case"
  (interactive)
  (save-excursion
    (apply 'evil-upcase (evil-inner-word))))

(defun my-lower-case ()
  "Make lower case"
  (interactive)
  (save-excursion
    (apply 'evil-downcase (evil-inner-word))))

(defun my-split-word ()
  "Split word into characters on separate lines"
  (interactive)
  (my-delete-word)
  (evil-open-below 1)
  (evil-force-normal-state)
  (evil-paste-after nil nil)
  (evil-beginning-of-line)
  (while (not (eolp))
    (evil-forward-char)
    (newline)
    (evil-force-normal-state)))

(defun my-delete-other-words ()
  "Delete other words in line"
  (interactive)
  (my-delete-word)
  (evil-open-below 1)
  (evil-force-normal-state)
  (evil-paste-after nil nil)
  (evil-previous-line)
  (my-delete-line))

(defun my-rotate-chars-right-in-word ()
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

(defun my-rotate-chars-left-in-word ()
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

(defun my-scroll-jump-words-backward ()
  "Scroll jump back across words."
  (interactive)
  (evil-backward-WORD-begin 3))

(defun my-scroll-jump-words-forward ()
  "Scroll jump forward across words."
  (interactive)
  (evil-forward-WORD-begin 3))

(defun my-first-word ()
  "Jump backward to the first word in the paragraph."
  (interactive)
  (evil-backward-paragraph)
  (evil-forward-WORD-begin))

(defun my-last-word ()
  "Jump forward to the last word in the paragraph."
  (interactive)
  (evil-forward-paragraph)
  (evil-backward-WORD-begin))

(defun my-add-to-word-after ()
  "Add to the end of this word."
  (interactive)
  (evil-forward-WORD-end)
  (evil-insert-state))

(defun my-add-word-after ()
  "Add a word after this one."
  (interactive)
  (evil-forward-WORD-begin)
  (insert " ")
  (backward-char)
  (evil-insert-state))

(defun my-add-to-word-before ()
  "Add to the beginning of this word."
  (interactive)
  (evil-backward-WORD-begin)
  (evil-insert-state))


(defhydra hydra-word (:idle 1.0
                      :columns 2
                      :post (chimera-hydra-portend-exit chimera-word-mode t)
                      :after-exit (chimera-hydra-signal-exit chimera-word-mode
                                                             #'chimera-handle-hydra-exit))
  "Word mode"
  ("h" evil-backward-WORD-begin "backward")
  ("j" evil-next-line "down")
  ("k" evil-previous-line "up")
  ("l" evil-forward-WORD-begin "forward")
  ("C-h" my-scroll-jump-words-backward "backward")
  ("C-j" my-scroll-jump-words-forward "down")
  ("C-k" my-scroll-jump-words-backward "up")
  ("C-l" my-scroll-jump-words-forward "forward")
  ("C-S-h" my-rotate-chars-left-in-word "rotate chars left")
  ("C-S-l" my-rotate-chars-right-in-word "rotate chars right")
  ("M-h" my-first-word "first word")
  ("M-l" my-last-word "last word")
  ("H" my-move-word-backward "move left")
  ("L" my-move-word-forward "move right")
  ("J" my-move-word-down "move down")
  ("K" my-move-word-up "move up")
  ("x" my-delete-word "delete")
  ("c" my-change-word "change" :exit t)
  ("a" my-add-to-word-after "append" :exit t)
  ("i" my-add-to-word-before "insert" :exit t)
  ("A" my-add-word-after "add after" :exit t)
  ("I" my-add-word-before "add before" :exit t)
  ("~" my-toggle-case "toggle case")
  ("U" my-upper-case "upper case")
  ("u" my-lower-case "lower case")
  ("s" my-split-word "split into characters")
  ("s-r" my-delete-word "delete" :exit t)
  ("s-o" my-delete-other-words "delete other words" :exit t)
  ("?" dictionary-lookup-definition "lookup in dictionary" :exit t)
  ("H-m" eem-toggle-menu "show/hide this menu")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-word-mode-entry-hook nil
  "Entry hook for epistemic word mode.")

(defvar chimera-word-mode-exit-hook nil
  "Exit hook for epistemic word mode.")

(defvar chimera-word-mode
  (make-chimera-mode :name "word"
                     :enter #'hydra-word/body
                     :entry-hook 'evil-word-state-entry-hook
                     :exit-hook 'evil-word-state-exit-hook))


(provide 'eem-word-mode)
