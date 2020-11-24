(require 'chimera)
(require 'chimera-hydra)

(evil-define-state text
  "Text state."
  :tag " <A> "
  :message "-- TEXT --"
  :enable (normal))


(defhydra hydra-text (:columns 2
                      :post (chimera-hydra-portend-exit chimera-text-mode t)
                      :after-exit (chimera-hydra-signal-exit chimera-text-mode
                                                             #'chimera-handle-hydra-exit))
  "Text mode"
  ("z" evil-fill-and-move "justify" :exit t)
  ("s-z" evil-fill-and-move "justify" :exit t)
  ("i" nil "exit" :exit t)
  ("H-m" eem-toggle-menu "show/hide this menu")
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


(provide 'eem-text-mode)
