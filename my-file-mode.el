;; From: https://www.emacswiki.org/emacs/MarkCommands#toc4
(defun unpop-to-mark-command ()
    "Unpop off mark ring. Does nothing if mark ring is empty."
    (interactive)
    (when mark-ring
      (let ((pos (marker-position (car (last mark-ring)))))
        (if (not (= (point) pos))
            (goto-char pos)
          (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
          (set-marker (mark-marker) pos)
          (setq mark-ring (nbutlast mark-ring))
          (goto-char (marker-position (car (last mark-ring))))))))

(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))

(defhydra hydra-file (:idle 1.0
                      :columns 2
                      :body-pre (evil-file-state))
  "File mode"
  ("h" evil-backward-char "backward")
  ("j" evil-next-line "down")
  ("k" evil-previous-line "up")
  ("l" evil-forward-char "forward")
  ("H" evil-goto-first-line "beginning")
  ("L" evil-goto-line "end")
  ("C-h" xah-pop-local-mark-ring "previous mark")
  ("C-l" unpop-to-mark-command "next mark")
  ("i" nil "exit" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-f") 'hydra-file/body)

(provide 'my-file-mode)
