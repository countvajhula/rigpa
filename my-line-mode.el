;; other possibilities:
;; - take number as arg
;; - reflect as a single evil command
;; - rotate words right/left
;; - > indent
;; - delete other lines
;; - copy line
;; similarly for "region-mode", possibly by invoking multiple cursors

(defun my-move-line-down (&optional count)
  "Move line down"
  (interactive)
  (unless count (setq count 1))
  (evil-next-line)
  (transpose-lines count)
  (evil-previous-line))

(defun my-move-line-up (&optional count)
  "Move line up"
  (interactive)
  (unless count (setq count 1))
  (transpose-lines count)
  (evil-previous-line 2))

(defun my-move-line-left (&optional count)
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

(defun my-move-line-right (&optional count)
  "Move line right"
  (interactive)
  (unless count (setq count 1))
  (save-excursion
    (evil-first-non-blank)
    (insert-char #x20 count)))

(defun my-move-line-far-left ()
  "Move line far left"
  (interactive)
  (save-excursion
    (evil-first-non-blank)
    (evil-delete (line-beginning-position)
                 (point)
                 (quote exclusive)
                 nil
                 nil)))

(defun my-move-line-far-right ()
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
      (my-move-line-far-left)
      (my-move-line-right line-position))))

(defun my-move-line-very-bottom ()
  "Move line to bottom"
  (interactive)
  (evil-execute-in-normal-state)
  (execute-kbd-macro (kbd ":.m$")))

(defun my-move-line-very-top ()
  "Move line to top"
  (interactive)
  (evil-execute-in-normal-state)
  (execute-kbd-macro (kbd ":.m0")))

(defun my-delete-line ()
  "Delete line"
  (interactive)
  (let* ((line-start-position (line-beginning-position))
         (line-end-position (if (eobp)
                                (line-end-position)
                              (+ 1 (line-end-position)))))
    (evil-delete-whole-line line-start-position
                            line-end-position
                            (quote line)
                            nil)))

(defun my-flashback ()
  "Flashback to prev line"
  (interactive)
  (evil-goto-mark-line ?'))

(defun my-split-line ()
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

(defun my-pulverize-line ()
  "Split on every character"
  (interactive)
  (evil-beginning-of-line)
  (while (not (eolp))
    (evil-forward-char)
    (newline)
    (evil-force-normal-state)))

(defun my-line-info ()
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

(defun my-toggle-comment-line ()
  "Comment / uncomment line"
  (interactive)
  (comment-line 1))

(defun my-yank-line ()
  "Yank (copy) line"
  (interactive)
  (evil-yank-line (line-beginning-position) (line-end-position) 'line nil))

(defun my-change-line ()
  "Change line"
  (interactive)
  (evil-change-whole-line (line-beginning-position)
                          (+ 1 (line-end-position))
                          (quote line)
                          nil))

(defun my-indent-line-left ()
  "Reduce line indent"
  (interactive)
  (indent-rigidly-left-to-tab-stop (line-beginning-position)
                                   (line-end-position)))

(defun my-indent-line-right ()
  "Increase line indent"
  (interactive)
  (indent-rigidly-right-to-tab-stop (line-beginning-position)
                                    (line-end-position)))

(defhydra hydra-line (:idle 1.0
                      :columns 4
                      :body-pre (evil-line-state))
  "Line mode"
  ("h" evil-previous-line "previous")
  ("j" evil-next-line "next")
  ("k" evil-previous-line "previous")
  ("l" evil-next-line "next")
  ("C-j" my-jump-down "jump down")
  ("C-k" my-jump-up "jump up")
  ("H" my-move-line-left "move left")
  ("J" my-move-line-down "move down")
  ("K" my-move-line-up "move up")
  ("L" my-move-line-right "move right")
  ("C-." my-indent-line-right "indent right")
  ("C-," my-indent-line-left "indent left")
  ("M-H" my-move-line-far-left "move to far left")
  ("M-J" my-move-line-very-bottom "move to bottom")
  ("M-K" my-move-line-very-top "move to top")
  ("M-L" my-move-line-far-right "move to far right")
  ("x" my-delete-line "delete")
  ("c" my-change-line "change")
  ("s-l" indent-according-to-mode "autoindent")
  ("o" my-flashback "flashback")
  ("s" my-split-line "split by word")
  ("v" my-pulverize-line "pulverize")
  ("y" my-yank-line "yank (copy)")
  (";" my-toggle-comment-line "toggle comment")
  ("i" my-line-info "info" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-l") 'hydra-line/body)

(provide 'my-line-mode)
