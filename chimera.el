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
    (add-hook exit-hook #'eem-remember-or-recall)
    (add-hook entry-hook #'eem-reconcile-level)))

(defun eem-unregister-mode (mode)
  "Unregister MODE-NAME."
  (let ((entry-hook (chimera-mode-entry-hook mode))
        (exit-hook (chimera-mode-exit-hook mode)))
    (remove-hook exit-hook #'eem-remember-or-recall)
    (remove-hook entry-hook #'eem-reconcile-level)))

(defvar chimera-evil-states
  (list "normal" "insert" "emacs"))

(defvar chimera-hook-modes
  nil)

(defun chimera-enter-mode (mode-name)
  "Enter MODE-NAME."
  (interactive)
  (message "entering mode %s" mode-name)
  ;; maybe better to lookup modes by name in a table
  (let ((mode (symbol-value (intern (concat "chimera-" mode-name "-mode")))))
    ;; call a function (perform-entry-actions ...) that
    ;; handles any provider-specific jankiness, like checking
    ;; for hydras that didn't exit cleanly, and perform their
    ;; exit actions (which should be in a dedicated function
    ;; that can be called from here as well as the original
    ;; spot in the hydra exit lifecycle phase).
    (unless (member mode-name chimera-evil-states)
      (let ((evil-state-entry (intern (concat "evil-" mode-name "-state"))))
        (funcall evil-state-entry)
        (message "changed to %s state" mode-name)))
    (funcall (chimera-mode-enter mode))
    (when (member mode-name chimera-hook-modes)
      ;; for now, we rely on evil hooks for all modes (incl.
      ;; hydra-based ones), and this should never be called.
      ;; probably incorporate an optional flag in the mode struct
      ;; to indicate hooks are managed elsewhere, instead,
      ;; `responsible-for-hooks` or something
      (message "Running entry hooks for %s mode" mode-name)
      (run-hooks (chimera-mode-entry-hook mode))))
  (message "entered mode %s" mode-name))

(defun chimera-exit-mode (mode-name)
  "Exit (interrupt) MODE-NAME."
  (interactive)
  (let ((mode (symbol-value (intern (concat "chimera-" mode-name "-mode")))))
    (unless (member mode-name chimera-evil-states)
      (funcall (chimera-mode-exit mode)))))

(defun chimera-handle-hydra-exit (mode-name)
  "Adapter helper for hydra to call hooks upon exit."
  (when (equal (symbol-name evil-state) mode-name)
    ;; hydra has exited but we haven't gone to a new state.
    ;; This means limbo, and we need to enter an appropriate
    ;; state for the buffer here
    ;; although, should we do nothing if current mode is
    ;; already in the tower?
    (message "hydra for mode %s exited into limbo; entering an appropriate state..."
             mode-name)
    (eem--enter-appropriate-mode))
  (when (member mode-name chimera-hook-modes)
    (let ((mode (symbol-value
                 (intern
                  ;; TODO: don't rely on a particular name being present
                  (concat "chimera-" mode-name "-mode")))))
      (run-hooks (chimera-mode-exit-hook mode)))))

(provide 'chimera)
;;; chimera.el ends here
