(cl-defstruct chimera-mode
  "Specification for a mode."
  name
  (enter :documentation "Primitive mode entry function.")
  (exit nil :documentation "Primitive mode exit function.") ; we don't need to rely on exit being defined
  (entry-hook nil)
  (exit-hook nil))

(defun eem-register-mode (mode)
  "Register MODE-NAME for use with epistemic mode."
  (let ((entry-hook (chimera-mode-entry-hook mode))
        (exit-hook (chimera-mode-exit-hook mode)))
    (add-hook exit-hook #'eem-set-mode-recall)
    (add-hook entry-hook #'eem-reconcile-level)))

(defun eem-unregister-mode (mode)
  "Unregister MODE-NAME."
  (let ((entry-hook (chimera-mode-entry-hook mode))
        (exit-hook (chimera-mode-exit-hook mode)))
    (remove-hook exit-hook #'eem-set-mode-recall)
    (remove-hook entry-hook #'eem-reconcile-level)))

(defvar chimera-evil-states
  (list "normal" "insert" "emacs"))

(defun chimera-enter-mode (mode-name)
  "Enter MODE-NAME."
  (interactive)
  (let ((evil-state-entry (intern (concat "evil-" mode-name "-state")))
        ;; maybe better to lookup modes by name
        (mode (symbol-value (intern (concat "chimera-" mode-name "-mode")))))
    (unless (member mode-name chimera-evil-states)
      (funcall evil-state-entry))
    (funcall (chimera-mode-enter mode))
    (unless (member mode-name chimera-evil-states)
      ;; probably incorporate an optional flag in the struct
      ;; to indicate hooks are managed elsewhere, instead
      ;; `responsible-for-hooks` or something
      (run-hooks (chimera-mode-entry-hook mode)))))

(defun chimera-exit-mode (mode-name)
  "Exit (interrupt) MODE-NAME."
  (interactive)
  (let ((mode (symbol-value (intern (concat "chimera-" mode-name "-mode")))))
    (unless (member mode-name chimera-evil-states)
      (funcall (chimera-mode-exit mode)))))

(provide 'chimera)
;;; chimera.el ends here
