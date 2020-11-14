(require 'lithium)

(defvar lithium-symex-mode-entry-hook nil
  "Entry hook for epistemic symex mode.")

(defvar lithium-symex-mode-exit-hook nil
  "Exit hook for epistemic symex mode.")

(defvar lithium-symex-mode
  (make-lithium-mode :name "symex"
                     :enter #'symex-mode-interface
                     :entry-hook 'lithium-symex-mode-entry-hook
                     :exit-hook 'lithium-symex-mode-exit-hook))

;; register mode with the epistemic framework
(eem-register-mode lithium-symex-mode)


(provide 'eem-symex-mode)
;;; eem-symex-mode.el ends here
