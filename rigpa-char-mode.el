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

(defun rigpa-char-info ()
  "Info on character"
  (interactive)
  (what-cursor-position))

(defun rigpa-char-delete ()
  "Delete character"
  (interactive)
  (evil-delete-char (point)
                    (+ (point) 1)
                    (quote exclusive) nil))

;; TODO: seems to vary depending on the value of evil-move-cursor-back
(defun rigpa-char-move-left (&optional superlative)
  "Move character left"
  (interactive)
  (when (not (bolp))
    (rigpa-char-delete)
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

(defun rigpa-char-move-right (&optional superlative)
  "Move character right"
  (interactive)
  (when (not (eolp))
    (rigpa-char-delete)
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

(defun rigpa-char-move-down (&optional superlative)
  "Move character down"
  (interactive)
  (rigpa-char-delete)
  (cond ((eq superlative nil) (evil-next-line))
        ((eq superlative 'more) (evil-next-line 3))
        ((eq superlative 'most) (evil-forward-paragraph)))
  (evil-paste-before nil nil))

(defun rigpa-char-move-up (&optional superlative)
  "Move character up"
  (interactive)
  (rigpa-char-delete)
  (cond ((eq superlative nil) (evil-previous-line))
        ((eq superlative 'more) (evil-previous-line 3))
        ((eq superlative 'most) (evil-backward-paragraph)))
  (evil-paste-before nil nil))

(defun rigpa-char-change ()
  "Change character"
  (interactive)
  (evil-substitute (point)
                   (+ (point) 1)
                   (quote exclusive)
                   nil))

(defun rigpa-char-yank ()
  "Yank (copy) character"
  (interactive)
  (evil-yank-characters (point) (+ (point) 1)))

(defun rigpa-char-toggle-case ()
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
  ("H" rigpa-char-move-left "move left")
  ("J" rigpa-char-move-down "move down")
  ("K" rigpa-char-move-up "move up")
  ("L" rigpa-char-move-right "move right")
  ("C-S-h" (lambda () (interactive) (rigpa-char-move-left 'more)) "move left more")
  ("C-S-j" (lambda () (interactive) (rigpa-char-move-down 'more)) "move down more")
  ("C-S-k" (lambda () (interactive) (rigpa-char-move-up 'more)) "move up more")
  ("C-S-l" (lambda () (interactive) (rigpa-char-move-right 'more)) "move right more")
  ("M-H" (lambda () (interactive) (rigpa-char-move-left 'most)) "move to far left")
  ("M-J" (lambda () (interactive) (rigpa-char-move-down 'most)) "move to bottom")
  ("M-K" (lambda () (interactive) (rigpa-char-move-up 'most)) "move to top")
  ("M-L" (lambda () (interactive) (rigpa-char-move-right 'most)) "move to far right")
  ("c" rigpa-char-change "change" :exit t)
  ("y" rigpa-char-yank "yank (copy)" :exit t)
  ("x" rigpa-char-delete "delete")
  ("~" rigpa-char-toggle-case "toggle case")
  ("i" rigpa-char-info "info" :exit t)
  ("?" rigpa-char-info "info" :exit t)
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-char-mode-entry-hook nil
  "Entry hook for rigpa char mode.")

(defvar chimera-char-mode-exit-hook nil
  "Exit hook for rigpa char mode.")

(defvar chimera-char-mode
  (make-chimera-mode :name "char"
                     :enter #'hydra-char/body
                     :pre-entry-hook 'chimera-char-mode-entry-hook
                     :post-exit-hook 'chimera-char-mode-exit-hook
                     :entry-hook 'evil-char-state-entry-hook
                     :exit-hook 'evil-char-state-exit-hook))


(provide 'rigpa-char-mode)
