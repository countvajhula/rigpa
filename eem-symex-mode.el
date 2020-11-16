(require 'chimera)

(defvar chimera-symex-mode-entry-hook nil
  "Entry hook for epistemic symex mode.")

(defvar chimera-symex-mode-exit-hook nil
  "Exit hook for epistemic symex mode.")

(defvar chimera-symex-mode
  (make-chimera-mode :name "symex"
                     :enter #'symex-mode-interface
                     :entry-hook 'chimera-symex-mode-entry-hook
                     :exit-hook 'chimera-symex-mode-exit-hook))

;; register mode with the epistemic framework
(eem-register-mode chimera-symex-mode)


(provide 'eem-symex-mode)
;;; eem-symex-mode.el ends here
