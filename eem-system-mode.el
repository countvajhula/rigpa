(require 'chimera)
(require 'eem-mode-adapter-hydra)

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
                        :post (eem-hydra-flag-mode-exit "system" t)
                        :after-exit (eem-hydra-signal-exit "system" #'eem-handle-mode-exit))
  "System information"
  ("b" my-battery-life "show power info including battery life")
  ("s-i" my-battery-life "show power info including battery life")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-system-mode-entry-hook nil
  "Entry hook for epistemic system mode.")

(defvar chimera-system-mode-exit-hook nil
  "Exit hook for epistemic system mode.")

(defvar chimera-system-mode
  (make-chimera-mode :name "system"
                     :enter #'hydra-system/body
                     :entry-hook 'chimera-system-mode-entry-hook
                     :exit-hook 'chimera-system-mode-exit-hook))

;; register mode with the epistemic framework
(eem-register-mode chimera-system-mode)


(provide 'eem-system-mode)
