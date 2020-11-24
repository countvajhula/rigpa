(require 'chimera)
(require 'chimera-hydra)

(evil-define-state char
  "Char state."
  :tag " <X> "
  :message "-- CHAR --"
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

(defun my-move-char-left (&optional superlative)
  "Move character left"
  (interactive)
  (when (not (bolp))
    (my-delete-char)
    (let ((at-eol (eolp)))
      (cond ((eq superlative nil) (evil-backward-char))
            ((eq superlative 'more) (evil-backward-char 3))
            ((eq superlative 'most) (evil-beginning-of-line)))
      (if at-eol
          ;; for some reason delete-char doesn't update point
          ;; while in hydra at EOL, so the handling here
          ;; is different than it otherwise would be
          (if (bolp)
              (evil-paste-before nil nil)
            (progn (evil-paste-after nil nil)
                   (backward-char)))
        (evil-paste-before nil nil)))))

(defun my-move-char-right (&optional superlative)
  "Move character right"
  (interactive)
  (when (not (eolp))
    (my-delete-char)
    (cond ((eq superlative 'more)
           (condition-case nil
               (evil-forward-char 2)
             (error nil)))
          ((eq superlative 'most) (evil-end-of-line)))
    (evil-paste-after nil nil)
    ;; Note: The above is sufficient when this command is run
    ;; interactively via M-x. But when run via the hydra, it
    ;; moves point forward an extra character. Not sure why this
    ;; happens but since hydra is the main entry point to this,
    ;; adding the line below for usage via hydra.
    (backward-char)))

(defun my-move-char-down (&optional superlative)
  "Move character down"
  (interactive)
  (my-delete-char)
  (cond ((eq superlative nil) (evil-next-line))
        ((eq superlative 'more) (evil-next-line 3))
        ((eq superlative 'most) (evil-forward-paragraph)))
  (evil-paste-before nil nil))

(defun my-move-char-up (&optional superlative)
  "Move character up"
  (interactive)
  (my-delete-char)
  (cond ((eq superlative nil) (evil-previous-line))
        ((eq superlative 'more) (evil-previous-line 3))
        ((eq superlative 'most) (evil-backward-paragraph)))
  (evil-paste-before nil nil))

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

(defhydra hydra-char (:columns 4
                      :post (chimera-hydra-portend-exit chimera-char-mode t)
                      :after-exit (chimera-hydra-signal-exit chimera-char-mode
                                                             #'chimera-handle-hydra-exit))
  "Character mode"
  ("h" evil-backward-char "left")
  ("j" evil-next-line "down")
  ("k" evil-previous-line "up")
  ("l" evil-forward-char "right")
  ("C-h" (lambda ()
           (interactive)
           (evil-backward-char 3)) "more left")
  ("C-j" (lambda ()
           (interactive)
           (evil-next-line 3)) "more down")
  ("C-k" (lambda ()
           (interactive)
           (evil-previous-line 3)) "more up")
  ("C-l" (lambda ()
           (interactive)
           (evil-forward-char 3)) "more right")
  ("M-h" (lambda ()
           (interactive)
           (evil-beginning-of-line)) "most left")
  ("M-j" (lambda ()
           (interactive)
           (evil-forward-paragraph)
           (evil-previous-line)) "most down")
  ("M-k" (lambda ()
           (interactive)
           (evil-backward-paragraph)
           (evil-next-line)) "most up")
  ("M-l" (lambda ()
           (interactive)
           (evil-end-of-line)) "most right")
  ("H" my-move-char-left "move left")
  ("J" my-move-char-down "move down")
  ("K" my-move-char-up "move up")
  ("L" my-move-char-right "move right")
  ("C-S-h" (lambda () (interactive) (my-move-char-left 'more)) "move left more")
  ("C-S-j" (lambda () (interactive) (my-move-char-down 'more)) "move down more")
  ("C-S-k" (lambda () (interactive) (my-move-char-up 'more)) "move up more")
  ("C-S-l" (lambda () (interactive) (my-move-char-right 'more)) "move right more")
  ("M-H" (lambda () (interactive (my-move-char-left 'most))) "move to far left")
  ("M-J" (lambda () (interactive) (my-move-char-down 'most)) "move to bottom")
  ("M-K" (lambda () (interactive) (my-move-char-up 'most)) "move to top")
  ("M-L" (lambda () (interactive) (my-move-char-right 'most)) "move to far right")
  ("c" my-change-char "change" :exit t)
  ("y" my-yank-char "yank (copy)" :exit t)
  ("x" my-delete-char "delete")
  ("~" my-toggle-case-char "toggle case")
  ("i" my-char-info "info" :exit t)
  ("?" my-char-info "info" :exit t)
  ("H-m" eem-toggle-menu "show/hide this menu")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-char-mode-entry-hook nil
  "Entry hook for epistemic char mode.")

(defvar chimera-char-mode-exit-hook nil
  "Exit hook for epistemic char mode.")

(defvar chimera-char-mode
  (make-chimera-mode :name "char"
                     :enter #'hydra-char/body
                     :entry-hook 'evil-char-state-entry-hook
                     :exit-hook 'evil-char-state-exit-hook))


(provide 'eem-char-mode)
