(require 'chimera)
(require 'chimera-hydra)

(evil-define-state history
  "History state."
  :tag " <C> "
  :message "-- x→o --"
  :enable (normal))

(defhydra hydra-history (:columns 2
                         ; maybe put body-pre in ad hoc entry
                         :body-pre (unless git-timemachine-mode (git-timemachine))
                         :post (chimera-hydra-portend-exit chimera-history-mode t)
                         :after-exit (chimera-hydra-signal-exit chimera-history-mode
                                                                #'chimera-handle-hydra-exit))
  "History mode"
  ("h" git-timemachine-show-previous-revision "previous")
  ("l" git-timemachine-show-next-revision "next")
  ("M-l" git-timemachine-show-current-revision "latest")
  ("b" git-timemachine-blame "annotate history ('blame')" :exit t)
  ("/" git-timemachine-show-revision-fuzzy "search")
  ("?" git-timemachine-show-commit "help (show commit)")
  ("q" git-timemachine-quit "return to the present" :exit t)
  ("H-m" eem-toggle-menu "show/hide this menu")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-history-mode-entry-hook nil
  "Entry hook for epistemic history mode.")

(defvar chimera-history-mode-exit-hook nil
  "Exit hook for epistemic history mode.")

(defvar chimera-history-mode
  (make-chimera-mode :name "history"
                     :enter #'hydra-history/body
                     :entry-hook 'evil-history-state-entry-hook
                     :exit-hook 'evil-history-state-exit-hook))


(provide 'eem-history-mode)
