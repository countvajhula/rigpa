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
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-text-mode-entry-hook nil
  "Entry hook for rigpa text mode.")

(defvar chimera-text-mode-exit-hook nil
  "Exit hook for rigpa text mode.")

(defvar chimera-text-mode
  (make-chimera-mode :name "text"
                     :enter #'hydra-text/body
                     :pre-entry-hook 'chimera-text-mode-entry-hook
                     :post-exit-hook 'chimera-text-mode-exit-hook
                     :entry-hook 'evil-text-state-entry-hook
                     :exit-hook 'evil-text-state-exit-hook))


(provide 'rigpa-text-mode)
