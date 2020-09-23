(evil-define-state text
  "Text state."
  :tag " <A> "
  :message "-- TEXT --"
  :entry-hook (hydra-text/body)
  :enable (normal))


(defhydra hydra-text (:color pink
                      :columns 2
                      :idle 1.0
                      :post (evil-normal-state))
  "Text mode"
  ("t" evil-fill-and-move "justify" :exit t)
  ("s-t" evil-fill-and-move "justify" :exit t)
  ("i" nil "exit" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-z") 'evil-text-state)

(provide 'eem-text-mode)
