(evil-define-state history
  "History state."
  :tag " <C> "
  :message "-- x→o --"
  :enable (normal))

(defhydra hydra-history (:color pink
                         :columns 2
                         :body-pre (progn (unless git-timemachine-mode (git-timemachine))
                                          (evil-history-state))
                         :idle 1.0
                         :post (evil-normal-state))
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

(global-set-key (kbd "s-g") 'hydra-history/body)

(provide 'eem-history-mode)
