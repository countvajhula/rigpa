(require 'chimera)

(defvar chimera-symex-mode-entry-hook nil
  "Entry hook for rigpa symex mode.")

(defvar chimera-symex-mode-exit-hook nil
  "Exit hook for rigpa symex mode.")

(defvar chimera-symex-mode
  (make-chimera-mode :name "symex"
                     :enter #'symex-mode-interface
                     :entry-hook 'evil-symex-state-entry-hook
                     :exit-hook 'evil-symex-state-exit-hook))


(provide 'rigpa-symex-mode)
;;; rigpa-symex-mode.el ends here
