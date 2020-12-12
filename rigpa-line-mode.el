;; other possibilities:
;; - take number as arg
;; - reflect as a single evil command
;; - rotate words right/left
;; - > indent
;; - delete other lines
;; - copy line
;; similarly for "region-mode", possibly by invoking multiple cursors

(require 'chimera)
(require 'chimera-hydra)

(evil-define-state line
  "Line state."
  :tag " <L> "
  :message "-- LINE --"
  :enable (normal))

(defun rigpa-line-move-down (&optional count)
  "Move line down"
  (interactive)
  (unless (save-excursion
            (end-of-line)
            (or (eobp)
                (save-excursion
                  (evil-next-line)
                  (eobp))))
    (unless count (setq count 1))
    (evil-next-line)
    (transpose-lines count)
    (evil-previous-line)))

(defun rigpa-line-move-up (&optional count)
  "Move line up"
  (interactive)
  (unless (save-excursion (beginning-of-line)
                          (bobp))
    (unless count (setq count 1))
    (transpose-lines count)
    (evil-previous-line 2)))

(defun rigpa-line-move-left (&optional count)
  "Move line left"
  (interactive)
  (unless count (setq count 1))
  (save-excursion
    (evil-first-non-blank)
    (setq starting-from (- (point) count))
    (if (< starting-from
           (line-beginning-position))
        (setq starting-from (line-beginning-position)))
    (evil-delete-backward-char starting-from
                               (point)
                               'exclusive
                               nil)))

(defun rigpa-line-move-right (&optional count)
  "Move line right"
  (interactive)
  (unless count (setq count 1))
  (save-excursion
    (evil-first-non-blank)
    (insert-char #x20 count)))

(defun rigpa-line-move-far-left ()
  "Move line far left"
  (interactive)
  (save-excursion
    (evil-first-non-blank)
    (evil-delete (line-beginning-position)
                 (point)
                 (quote exclusive)
                 nil
                 nil)))

(defun rigpa-line-move-far-right ()
  "Move line far right"
  (interactive)
  (save-excursion
    (evil-beginning-of-line)
    (unless (bobp)
      (evil-previous-line)
      (evil-first-non-blank)
      (setq line-position (- (point)
                             (line-beginning-position)))
      (evil-next-line)
      (rigpa-line-move-far-left)
      (rigpa-line-move-right line-position))))

(defun rigpa-line-move-very-bottom ()
  "Move line to bottom"
  (interactive)
  (evil-execute-in-normal-state)
  (execute-kbd-macro (kbd ":.m$")))

(defun rigpa-line-move-very-top ()
  "Move line to top"
  (interactive)
  (evil-execute-in-normal-state)
  (execute-kbd-macro (kbd ":.m0")))

(defun rigpa-line-delete ()
  "Delete line"
  (interactive)
  (let* ((line-start-position (line-beginning-position))
         (line-end-position (if (eobp)
                                (line-end-position)
                              (1+ (line-end-position)))))
    (evil-delete-whole-line line-start-position
                            line-end-position
                            (quote line)
                            nil)))

(defun rigpa-line-flashback ()
  "Flashback to prev line"
  (interactive)
  (evil-goto-mark-line ?'))

(defun rigpa-line-split ()
  "Split line on word separators"
  (interactive)
  (evil-beginning-of-line)
  (while (not (eolp))
    (unless (equal (- (line-end-position)
                      (line-beginning-position))
                   1)
      (evil-forward-word-end))
    (execute-kbd-macro (kbd "a"))
    (newline)
    (evil-force-normal-state)))

(defun rigpa-line-pulverize ()
  "Split on every character"
  (interactive)
  (evil-beginning-of-line)
  (while (not (eolp))
    (evil-forward-char)
    (newline)
    (evil-force-normal-state)))

(defun rigpa-line-info ()
  "Info about the line"
  (interactive)

  (defun line-length (n)
    "Length of the Nth line.
From: https://emacs.stackexchange.com/questions/17846/calculating-the-length-of-a-line-in-a-buffer"
    (save-excursion
      (goto-char (point-min))
      (if (zerop (forward-line (1- n)))
          (- (line-end-position)
             (line-beginning-position)))))
  (setq current-line-number (line-number-at-pos))
  (setq current-line-length (line-length current-line-number))
  (message "Line %d, length = %d" current-line-number current-line-length))

(defun rigpa-line-toggle-comment ()
  "Comment / uncomment line"
  (interactive)
  (comment-line 1))

(defun rigpa-line-yank ()
  "Yank (copy) line"
  (interactive)
  (evil-yank-line (line-beginning-position) (line-end-position) 'line nil))

(defun rigpa-line-change ()
  "Change line"
  (interactive)
  (evil-change-whole-line (line-beginning-position)
                          (+ 1 (line-end-position))
                          (quote line)
                          nil))

(defun rigpa-line-indent-left ()
  "Reduce line indent"
  (interactive)
  (indent-rigidly-left-to-tab-stop (line-beginning-position)
                                   (line-end-position)))

(defun rigpa-line-indent-right ()
  "Increase line indent"
  (interactive)
  (indent-rigidly-right-to-tab-stop (line-beginning-position)
                                    (line-end-position)))

(defun rigpa-line-insert-newline ()
  "Insert newline and reindent."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline-and-indent)))

(defun rigpa-line-append-newline ()
  "Append newline and reindent."
  (interactive)
  (save-excursion
    (forward-line)
    (newline-and-indent)))

(defun rigpa-line-join (&optional backwards)
  "Join lines."
  (interactive)
  (save-excursion
    (if backwards
        (progn (evil-previous-line)
               (if (current-line-empty-p)
                   (evil-join (line-beginning-position)
                              (1+ (line-beginning-position)))
                 (evil-join (line-beginning-position)
                            (line-end-position))))
      (evil-join (line-beginning-position)
       (line-end-position)))))

(defun rigpa-line-top ()
  (interactive)
  (evil-goto-line))

(defun rigpa-line-bottom ()
  (interactive)
  (evil-goto-line 1))

(defun rigpa-line-jump-down ()
  (interactive)
  (evil-next-line 9))

(defun rigpa-line-jump-up ()
  (interactive)
  (evil-previous-line 9))

(defhydra hydra-line (:columns 4
                      :post (chimera-hydra-portend-exit chimera-line-mode t)
                      :after-exit (chimera-hydra-signal-exit chimera-line-mode
                                                             #'chimera-handle-hydra-exit))
  "Line mode"
  ("h" evil-previous-line "previous")
  ("j" evil-next-line "next")
  ("k" evil-previous-line "previous")
  ("l" evil-next-line "next")
  ("C-j" rigpa-line-jump-down "jump down")
  ("C-k" rigpa-line-jump-up "jump up")
  ("M-j" rigpa-line-top "top line")
  ("M-k" rigpa-line-bottom "bottom line")
  ("H" rigpa-line-move-left "move left")
  ("J" rigpa-line-move-down "move down")
  ("K" rigpa-line-move-up "move up")
  ("L" rigpa-line-move-right "move right")
  ("C-." rigpa-line-indent-right "indent right")
  ("C-," rigpa-line-indent-left "indent left")
  ("M-H" rigpa-line-move-far-left "move to far left")
  ("M-J" rigpa-line-move-very-bottom "move to bottom")
  ("M-K" rigpa-line-move-very-top "move to top")
  ("M-L" rigpa-line-move-far-right "move to far right")
  ("x" rigpa-line-delete "delete")
  ("c" rigpa-line-change "change")
  ("s-l" indent-according-to-mode "autoindent")
  ("'" rigpa-line-flashback "flashback")
  ("s" rigpa-line-split "split by word")
  ("v" rigpa-line-pulverize "pulverize")
  ("y" rigpa-line-yank "yank (copy)")
  ("p" evil-paste-after "paste after")
  ("P" evil-paste-before "paste before")
  ("+" evil-open-above "add new line")
  ("i" evil-open-above "add new line")
  ("a" evil-open-below "add new line below")
  ("n" rigpa-line-insert-newline "insert newline")
  ("C-S-o" rigpa-line-append-newline "append newline")
  ("o" rigpa-line-join "join")
  ("O" (lambda ()
         (interactive)
         (rigpa-line-join t))
   "join backwards")
  (";" rigpa-line-toggle-comment "toggle comment")
  ("?" rigpa-line-info "info" :exit t)
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-line-mode-entry-hook nil
  "Entry hook for rigpa line mode.")

(defvar chimera-line-mode-exit-hook nil
  "Exit hook for rigpa line mode.")

(defvar chimera-line-mode
  (make-chimera-mode :name "line"
                     :enter #'hydra-line/body
                     :entry-hook 'evil-line-state-entry-hook
                     :exit-hook 'evil-line-state-exit-hook))


(provide 'rigpa-line-mode)
