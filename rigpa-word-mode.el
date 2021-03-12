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

(defun rigpa-word--define-evil-key (key fn)
  "Define an evil keybinding in rigpa word mode."
  (rigpa--define-evil-key key
                          fn
                          rigpa-word-mode-map))

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

(rigpa-word--define-evil-key "h"
                             #'rigpa-word-backward)

(rigpa-word--define-evil-key "j"
                             #'rigpa-word-down)

(rigpa-word--define-evil-key "k"
                             #'rigpa-word-up)

(rigpa-word--define-evil-key "l"
                             #'rigpa-word-forward)

;; (evil-define-key '(word visual operator) rigpa-word-mode-map
;;   (kbd "H")
;;   #'rigpa-word-move-backward)

;; (evil-define-key '(word visual operator) rigpa-word-mode-map
;;   (kbd "L")
;;   #'rigpa-word-move-forward)

;; (evil-define-key '(word visual operator) rigpa-word-mode-map
;;   (kbd "J")
;;   #'rigpa-word-move-down)

;; (evil-define-key '(word visual operator) rigpa-word-mode-map
;;   (kbd "K")
;;   #'rigpa-word-move-up)


(defun rigpa-word-move-backward ()
  "Move word backwards"
  (interactive)
  (evil-inner-word nil nil nil nil)
  (evil-backward-WORD-begin nil)
  (transpose-words 1)
  (evil-backward-WORD-begin 2))

(defun rigpa-word-move-forward ()
  "Move word forward"
  (interactive)
  (evil-forward-WORD-begin nil)
  (transpose-words 1))

(defun rigpa-word-move-down ()
  "Move word down"
  (interactive)
  (evil-inner-word nil nil nil nil)
  (setq word-end-position (point))
  (evil-backward-WORD-begin nil)
  (evil-delete (point) word-end-position 'exclusive nil nil)
  (evil-next-line)
  (evil-paste-before nil nil))

(defun rigpa-word-move-up ()
  "Move word up"
  (interactive)
  (evil-inner-word nil nil nil nil)
  (setq word-end-position (point))
  (evil-backward-WORD-begin nil)
  (evil-delete (point) word-end-position 'exclusive nil nil)
  (evil-previous-line)
  (evil-paste-before nil nil))

(defun rigpa-word-delete ()
  "Delete word"
  (interactive)
  (apply 'evil-delete (evil-inner-word)))

(defun rigpa-word-change ()
  "Change word"
  (interactive)
  (apply 'evil-change (evil-inner-word)))

(defun rigpa-word-toggle-case ()
  "Toggle case"
  (interactive)
  (save-excursion
    (apply 'evil-invert-case (evil-inner-word))))

(defun rigpa-word-upper-case ()
  "Make upper case"
  (interactive)
  (save-excursion
    (apply 'evil-upcase (evil-inner-word))))

(defun rigpa-word-lower-case ()
  "Make lower case"
  (interactive)
  (save-excursion
    (apply 'evil-downcase (evil-inner-word))))

(defun rigpa-word-split ()
  "Split word into characters on separate lines"
  (interactive)
  (rigpa-word-delete)
  (evil-open-below 1)
  (evil-force-normal-state)
  (evil-paste-after nil nil)
  (evil-beginning-of-line)
  (while (not (eolp))
    (evil-forward-char)
    (newline)
    (evil-force-normal-state)))

(defun rigpa-word-delete-others ()
  "Delete other words in line"
  (interactive)
  (rigpa-word-delete)
  (evil-open-below 1)
  (evil-force-normal-state)
  (evil-paste-after nil nil)
  (evil-previous-line)
  (rigpa-line-delete))

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


;; (defhydra hydra-word (:columns 2
;;                       :post (chimera-hydra-portend-exit chimera-word-mode t)
;;                       :after-exit (chimera-hydra-signal-exit chimera-word-mode
;;                                                              #'chimera-handle-hydra-exit))
;;   "Word mode"
;;   ("h" evil-backward-WORD-begin "backward")
;;   ("j" evil-next-line "down")
;;   ("k" evil-previous-line "up")
;;   ("l" evil-forward-WORD-begin "forward")
;;   ("C-h" rigpa-word-scroll-jump-backward "backward")
;;   ("C-j" rigpa-word-scroll-jump-forward "down")
;;   ("C-k" rigpa-word-scroll-jump-backward "up")
;;   ("C-l" rigpa-word-scroll-jump-forward "forward")
;;   ("C-S-h" rigpa-word-rotate-chars-left "rotate chars left")
;;   ("C-S-l" rigpa-word-rotate-chars-right "rotate chars right")
;;   ("M-h" rigpa-word-first-word "first word")
;;   ("M-l" rigpa-word-last-word "last word")
;;   ("H" rigpa-word-move-backward "move left")
;;   ("L" rigpa-word-move-forward "move right")
;;   ("J" rigpa-word-move-down "move down")
;;   ("K" rigpa-word-move-up "move up")
;;   ("x" rigpa-word-delete "delete")
;;   ("c" rigpa-word-change "change" :exit t)
;;   ("a" rigpa-word-add-to-end "append" :exit t)
;;   ("i" rigpa-word-add-to-beginning "insert" :exit t)
;;   ("A" rigpa-word-add-after "add after" :exit t)
;;   ("I" rigpa-word-add-before "add before" :exit t)
;;   ("~" rigpa-word-toggle-case "toggle case")
;;   ("U" rigpa-word-upper-case "upper case")
;;   ("u" rigpa-word-lower-case "lower case")
;;   ("s" rigpa-word-split "split into characters")
;;   ("s-r" rigpa-word-delete "delete" :exit t)
;;   ("s-o" rigpa-word-delete-others "delete other words" :exit t)
;;   ("?" dictionary-lookup-definition "lookup in dictionary" :exit t)
;;   ("H-m" rigpa-toggle-menu "show/hide this menu")
;;   ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
;;   ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

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
