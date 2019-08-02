(evil-define-state char
  "Char state."
  :tag " <X> "
  :message "-- CHAR --"
  :entry-hook (hydra-char/body)
  ;;:cursor ;; inherit from normal
  ;;:exit-hook ;; none
  ;;:suppress-keymap) ;; should be t, but probably inherits from normal
  :enable (normal))

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
  (when (not (bolp))
    (my-delete-char)
    (let ((at-eol (eolp)))
      (evil-backward-char)
      (if at-eol
          (evil-paste-after nil nil)
        (evil-paste-before nil nil)))))

(defun my-move-char-right ()
  "Move character right"
  (interactive)
  (when (not (eolp))
    (my-delete-char)
    (evil-paste-after nil nil)
    ;; Note: The above is sufficient when this command is run
    ;; interactively via M-x. But when run via the hydra, it
    ;; moves point forward an extra character. Not sure why this
    ;; happens but since hydra is the main entry point to this,
    ;; adding the line below for usage via hydra.
    (backward-char)))

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
                      :color pink
                      :post (evil-normal-state))
  "Character mode"
  ("h" evil-backward-char "left")
  ("j" evil-next-line "down")
  ("k" evil-previous-line "up")
  ("l" evil-forward-char "right")
  ("H" my-move-char-left "move left")
  ("J" my-move-char-down "move down")
  ("K" my-move-char-up "move up")
  ("L" my-move-char-right "move right")
  ("M-H" my-move-char-far-left "move to far left")
  ("M-J" my-move-char-very-bottom "move to bottom")
  ("M-K" my-move-char-very-top "move to top")
  ("M-L" my-move-char-far-right "move to far right")
  ("c" my-change-char "change" :exit t)
  ("y" my-yank-char "yank (copy)" :exit t)
  ("x" my-delete-char "delete")
  ("~" my-toggle-case-char "toggle case")
  ("i" my-char-info "info" :exit t)
  ("?" my-char-info "info" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-x") 'evil-char-state)

(provide 'eem-char-mode)
