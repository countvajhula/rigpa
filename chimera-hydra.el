
(defun chimera--hydra-for-state (mode-name)
  (intern (concat "hydra-" mode-name)))

(defun chimera--hydra-set-flag (hydra flag &optional value)
  "Set a FLAG on the HYDRA with the value VALUE.

If no VALUE is provided, this clears the flag."
  (hydra-set-property hydra flag value)
  (if value
      (message "updated %s flag on %s to %s" flag hydra value)
    (message "cleared %s flag on %s" flag hydra)))

(defun chimera-hydra-portend-exit (mode &optional value)
  "Set a mode exit flag to indicate cleanup operations need to be performed."
  (let* ((mode-name (chimera-mode-name mode))
         (hydra (chimera--hydra-for-state mode-name)))
    (chimera--hydra-set-flag hydra :exiting value)))

(defun chimera-hydra-signal-exit (mode callback)
  "Helper function to witness hydra exit and notify epistemic mode."
  (let* ((mode-name (chimera-mode-name mode))
         (hydra (chimera--hydra-for-state mode-name)))
    (when (hydra-get-property hydra :exiting)
      (funcall callback mode)
      (chimera--hydra-set-flag hydra :exiting))))

(defun chimera-handle-hydra-exit (mode)
  "Adapter helper for hydra to call hooks upon exit."
  (let ((mode-name (chimera-mode-name mode)))
    (message "Hydra %s exited." mode-name)
    (when (equal (symbol-name evil-state) mode-name)
      ;; hydra has exited but we haven't gone to a new state.
      ;; This means limbo, and we need to enter an appropriate
      ;; state for the buffer here
      ;; although, should we do nothing if current mode is
      ;; already in the tower?
      ;; [doing this for now to fix symex margins issue, but
      ;; not sure exactly what is happening there]
      (message "hydra for mode %s exited into limbo." mode-name)
      (unless (eem-ensemble-member-position-by-name (eem--local-tower)
                                                    (symbol-name evil-state))
        (message "exiting limbo by entering an appropriate state...")
        (eem--enter-appropriate-mode)))
    (when (chimera-mode-manage-hooks mode)
      (message "Running exit hooks for %s mode" mode-name)
      (run-hooks (chimera-mode-exit-hook mode)))))

(provide 'chimera-hydra)
