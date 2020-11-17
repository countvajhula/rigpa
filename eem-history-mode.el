(require 'chimera)
(require 'eem-mode-adapter-hydra)

(evil-define-state history
  "History state."
  :tag " <C> "
  :message "-- x→o --"
  :enable (normal))

(defhydra hydra-history (:color pink
                         :columns 2
                         ; maybe put body-pre in ad hoc entry
                         :body-pre (unless git-timemachine-mode (git-timemachine))
                         :idle 1.0
                         :post (eem-hydra-flag-mode-exit "history" t)
                         :after-exit (eem-hydra-signal-exit "history" #'chimera-handle-hydra-exit))
  "History mode"
  ("h" git-timemachine-show-previous-revision "previous")
  ("l" git-timemachine-show-next-revision "next")
  ("M-l" git-timemachine-show-current-revision "latest")
  ("b" git-timemachine-blame "annotate history ('blame')" :exit t)
  ("/" git-timemachine-show-revision-fuzzy "search")
  ("?" git-timemachine-show-commit "help (show commit)")
  ("q" git-timemachine-quit "return to the present" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-history-mode-entry-hook nil
  "Entry hook for epistemic history mode.")

(defvar chimera-history-mode-exit-hook nil
  "Exit hook for epistemic history mode.")

(defvar chimera-history-mode
  (make-chimera-mode :name "history"
                     :enter #'hydra-history/body
                     :entry-hook 'chimera-history-mode-entry-hook
                     :exit-hook 'chimera-history-mode-exit-hook))

;; register mode with the epistemic framework
(eem-register-mode chimera-history-mode)


(provide 'eem-history-mode)
