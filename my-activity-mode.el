(setq my-accumulate-buffer-name "MY-CLIPBOARD")

(defun my-yank-and-accumulate ()
  "'Yank'/accumulate text in a temporary buffer."
  (interactive)
  (unless (get-buffer my-accumulate-buffer-name)
    (my-new-empty-buffer my-accumulate-buffer-name))
  (append-to-buffer my-accumulate-buffer-name
                    (region-beginning)
                    (region-end))
  (deactivate-mark))

(defun my-paste-and-clear ()
  "Paste contents of paste buffer and empty it."
  (interactive)
  (insert-buffer my-accumulate-buffer-name)
  (kill-buffer my-accumulate-buffer-name))

(defhydra hydra-activity (:idle 1.0
                          :columns 2
                          :body-pre (evil-activity-state))
  "Activity mode"
  ("h" goto-last-change "previous change")
  ("j" goto-last-change-reverse "next change")
  ("k" goto-last-change "previous change")
  ("l" goto-last-change-reverse "next change")
  ("y" my-yank-and-accumulate "yank and accumulate") ;; TODO: these don't work via hydra atm
  ("p" my-paste-and-clear "paste and clear")
  ("i" my-noop "exit" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-a") 'hydra-activity/body)

(provide 'my-activity-mode)
