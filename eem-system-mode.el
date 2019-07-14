(evil-define-state system
  "System state."
  :tag " <S> "
  :message "-- SYSTEM --"
  :entry-hook (hydra-system/body)
  :enable (normal))

(defun my-battery-life ()
  "Show power info including battery life
   (Mac-specific, at the moment)."
  (interactive)
  (display-message-or-buffer (shell-command-to-string "pmset -g batt")))

(defhydra hydra-system (:exit t
                        :post (evil-normal-state))
  "System information"
  ("b" my-battery-life "show power info including battery life")
  ("s-i" my-battery-life "show power info including battery life")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

;; access the system menu via a "body" keybinding
(global-set-key (kbd "s-i") 'evil-system-state)

(provide 'eem-system-mode)
