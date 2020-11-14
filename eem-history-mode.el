(require 'lithium)

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
                         :post (eem--update-mode-exit-flag "history" t)
                         :after-exit (eem-hydra-signal-exit "history"))
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

(defvar lithium-history-mode-entry-hook nil
  "Entry hook for epistemic history mode.")

(defvar lithium-history-mode-exit-hook nil
  "Exit hook for epistemic history mode.")

(defvar lithium-history-mode
  (make-lithium-mode :name "history"
                     :enter #'hydra-history/body
                     :entry-hook 'lithium-history-mode-entry-hook
                     :exit-hook 'lithium-history-mode-exit-hook))

;; register mode with the epistemic framework
(eem-register-mode lithium-history-mode)


(provide 'eem-history-mode)
