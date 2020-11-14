(require 'lithium)

(evil-define-state text
  "Text state."
  :tag " <A> "
  :message "-- TEXT --"
  :enable (normal))


(defhydra hydra-text (:color pink
                      :columns 2
                      :idle 1.0
                      :post (eem--update-mode-exit-flag "text" t)
                      :after-exit (eem-hydra-signal-exit "text"))
  "Text mode"
  ("z" evil-fill-and-move "justify" :exit t)
  ("s-z" evil-fill-and-move "justify" :exit t)
  ("i" nil "exit" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(defvar lithium-text-mode-entry-hook nil
  "Entry hook for epistemic text mode.")

(defvar lithium-text-mode-exit-hook nil
  "Exit hook for epistemic text mode.")

(defvar lithium-text-mode
  (make-lithium-mode :name "text"
                     :enter #'hydra-text/body
                     :entry-hook 'lithium-text-mode-entry-hook
                     :exit-hook 'lithium-text-mode-exit-hook))

;; register mode with the epistemic framework
(eem-register-mode lithium-text-mode)


(provide 'eem-text-mode)
