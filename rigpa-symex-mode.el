(require 'chimera)

(defvar chimera-symex-mode-entry-hook nil
  "Entry hook for rigpa symex mode.")

(defvar chimera-symex-mode-exit-hook nil
  "Exit hook for rigpa symex mode.")

(defun rigpa--enable-symex-minor-mode ()
  "Enable symex minor mode."
  (symex-enable-editing-minor-mode))

(defun rigpa--disable-symex-minor-mode ()
  "Disable symex minor mode."
  (symex-disable-editing-minor-mode))

(defvar chimera-symex-mode
  (make-chimera-mode :name "symex"
                     :enter #'symex-mode-interface
                     :pre-entry-hook 'chimera-symex-mode-entry-hook
                     :post-exit-hook 'chimera-symex-mode-exit-hook
                     :entry-hook 'evil-symex-state-entry-hook
                     :exit-hook 'evil-symex-state-exit-hook))


(provide 'rigpa-symex-mode)
;;; rigpa-symex-mode.el ends here
