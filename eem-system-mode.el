(require 'chimera)
(require 'chimera-hydra)

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
                        :post (chimera-hydra-portend-exit chimera-system-mode t)
                        :after-exit (chimera-hydra-signal-exit chimera-system-mode
                                                               #'chimera-handle-hydra-exit))
  "System information"
  ("b" my-battery-life "show power info including battery life")
  ("s-i" my-battery-life "show power info including battery life")
  ("H-m" eem-toggle-menu "show/hide this menu" :exit nil)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-system-mode-entry-hook nil
  "Entry hook for epistemic system mode.")

(defvar chimera-system-mode-exit-hook nil
  "Exit hook for epistemic system mode.")

(defvar chimera-system-mode
  (make-chimera-mode :name "system"
                     :enter #'hydra-system/body
                     :entry-hook 'evil-system-state-entry-hook
                     :exit-hook 'evil-system-state-exit-hook))


(provide 'eem-system-mode)
