(evil-define-state system
  "System state."
  :tag " <S> "
  :message "-- SYSTEM --"
  :enable (normal))

(defun my-battery-life ()
  "Show power info including battery life
   (Mac-specific, at the moment)."
  (interactive)
  (display-message-or-buffer (shell-command-to-string "pmset -g batt")))

(defhydra hydra-system (:exit t
                        :body-pre (evil-system-state)
                        :post (eem--update-mode-exit-flag "system" t)
                        :after-exit (eem-hydra-signal-exit "system"))
  "System information"
  ("b" my-battery-life "show power info including battery life")
  ("s-i" my-battery-life "show power info including battery life")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))


(provide 'eem-system-mode)
