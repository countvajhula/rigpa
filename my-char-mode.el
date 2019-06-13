(defun my-char-info ()
  "Info on character"
  (interactive)
  (what-cursor-position))

(defun my-delete-char ()
  "Delete character"
  (interactive)
  (evil-delete-char (point)
                    (+ (point) 1)
                    (quote exclusive) nil))

(defun my-move-char-left ()
  "Move character left"
  (interactive)
  (my-delete-char)
  (evil-backward-char)
  (evil-paste-before nil nil))

(defun my-move-char-right ()
  "Move character right"
  (interactive)
  (my-delete-char)
  (evil-paste-after nil nil))

(defun my-move-char-down ()
  "Move character down"
  (interactive)
  (my-delete-char)
  (evil-next-line)
  (evil-paste-before nil nil))

(defun my-move-char-up ()
  "Move character up"
  (interactive)
  (my-delete-char)
  (evil-previous-line)
  (evil-paste-before nil nil))

(defun my-move-char-far-left ()
  "Move character far left"
  (interactive)
  (my-delete-char)
  (evil-beginning-of-line)
  (evil-paste-before nil nil))

(defun my-move-char-very-bottom ()
  "Move character to the bottom"
  (interactive)
  (my-delete-char)
  (evil-goto-line)
  (evil-paste-before nil nil))

(defun my-move-char-very-top ()
  "Move character to the top"
  (interactive)
  (my-delete-char)
  (evil-goto-first-line)
  (evil-paste-before nil nil))

(defun my-move-char-far-right ()
  "Move character far right"
  (interactive)
  (my-delete-char)
  (evil-end-of-line)
  (evil-paste-after nil nil))

(defun my-change-char ()
  "Change character"
  (interactive)
  (evil-substitute (point)
                   (+ (point) 1)
                   (quote exclusive)
                   nil))

(defun my-yank-char ()
  "Yank (copy) character"
  (interactive)
  (evil-yank-characters (point) (+ (point) 1)))

(defun my-toggle-case-char ()
  "Toggle upper-/lower-case"
  (interactive)
  (evil-invert-char (point) (+ (point) 1) (quote exclusive)))

(defhydra hydra-char (:idle 1.0
                      :columns 4
                      :body-pre (evil-char-state))
  "Character mode"
  ("h" evil-backward-char "left")
  ("j" evil-next-line "down")
  ("k" evil-previous-line "up")
  ("l" evil-forward-char "right")
  ("C-h" my-move-char-left "move left")
  ("C-j" my-move-char-down "move down")
  ("C-k" my-move-char-up "move up")
  ("C-l" my-move-char-right "move right")
  ("H" my-move-char-far-left "move to far left")
  ("J" my-move-char-very-bottom "move to bottom")
  ("K" my-move-char-very-top "move to top")
  ("L" my-move-char-far-right "move to far right")
  ("c" my-change-char "change" :exit t)
  ("y" my-yank-char "yank (copy)" :exit t)
  ("x" my-delete-char "delete")
  ("~" my-toggle-case-char "toggle case")
  ("i" my-char-info "info" :exit t)
  ("?" my-char-info "info" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-x") 'hydra-char/body)

(provide 'my-char-mode)
