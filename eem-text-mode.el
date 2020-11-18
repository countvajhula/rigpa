(require 'chimera)
(require 'eem-mode-adapter-hydra)

(evil-define-state text
  "Text state."
  :tag " <A> "
  :message "-- TEXT --"
  :enable (normal))


(defhydra hydra-text (:color pink
                      :columns 2
                      :idle 1.0
                      :post (eem-hydra-flag-mode-exit "text" t)
                      :after-exit (eem-hydra-signal-exit "text" #'chimera-handle-hydra-exit))
  "Text mode"
  ("z" evil-fill-and-move "justify" :exit t)
  ("s-z" evil-fill-and-move "justify" :exit t)
  ("i" nil "exit" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-text-mode-entry-hook nil
  "Entry hook for epistemic text mode.")

(defvar chimera-text-mode-exit-hook nil
  "Exit hook for epistemic text mode.")

(defvar chimera-text-mode
  (make-chimera-mode :name "text"
                     :enter #'hydra-text/body
                     :entry-hook 'evil-text-state-entry-hook
                     :exit-hook 'evil-text-state-exit-hook))

;; register mode with the epistemic framework
(eem-register-mode chimera-text-mode)


(provide 'eem-text-mode)
