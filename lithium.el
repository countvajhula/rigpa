(cl-defstruct lithium-mode
  "Specification for a mode."
  name
  (enter :documentation "Primitive mode entry function.")
  (exit nil :documentation "Primitive mode exit function.") ; we don't need to rely on exit being defined
  (entry-hook nil)
  (exit-hook nil))

(defun eem-register-mode (mode-name)
  "Register MODE-NAME for use with epistemic mode."
  (let* ((mode (symbol-value (intern (concat "lithium-" mode-name "-mode"))))
         (entry-hook (lithium-mode-entry-hook mode))
         (exit-hook (lithium-mode-exit-hook mode)))
    (add-hook exit-hook #'eem-jump-to-level-before)
    (add-hook entry-hook #'eem-jump-to-level-after)))

;; TODO: write mode abstractions for and register built-in evil states

(defun eem-unregister-mode (mode-name)
  "Unregister MODE-NAME."
  (let* ((mode (symbol-value (intern (concat "lithium-" mode-name "-mode"))))
         (entry-hook (lithium-mode-entry-hook mode))
         (exit-hook (lithium-mode-exit-hook mode)))
    (remove-hook exit-hook #'eem-jump-to-level-before)
    (remove-hook entry-hook #'eem-jump-to-level-after)))

(defun lithium-enter-mode (mode-name)
  "Enter MODE-NAME."
  (interactive)
  (let ((evil-state-entry (intern (concat "evil-" mode-name "-state")))
        ;; maybe better to lookup modes by name
        (mode (symbol-value (intern (concat "lithium-" mode-name "-mode")))))
    (funcall evil-state-entry)
    (run-hooks (lithium-mode-entry-hook mode))
    (funcall (lithium-mode-enter mode))))

(defun lithium-exit-mode (mode-name)
  "Exit (interrupt) MODE-NAME."
  (interactive)
  (let ((mode (symbol-value (intern (concat "lithium-" mode-name "-mode")))))
    (funcall (lithium-mode-exit mode))))

(provide 'lithium)
;;; lithium.el ends here
