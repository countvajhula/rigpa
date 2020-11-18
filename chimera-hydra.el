
(defun chimera--hydra-set-flag (hydra flag &optional value)
  "Set a FLAG on the HYDRA with the value VALUE.

If no VALUE is provided, this clears the flag."
  (hydra-set-property hydra flag value)
  (if value
      (message "updated %s flag on %s to %s" flag hydra value)
    (message "cleared %s flag on %s" flag hydra)))

(defun chimera-hydra-portend-exit (mode &optional value)
  "Set a mode exit flag to indicate cleanup operations need to be performed."
  (let ((hydra (intern (concat "hydra-" mode))))
    (chimera--hydra-set-flag hydra :exiting value)))

(defun eem-hydra-signal-exit (mode callback)
  "Helper function to witness hydra exit and notify epistemic mode."
  (let ((hydra (intern (concat "hydra-" mode))))
    (when (hydra-get-property hydra :exiting)
      (funcall callback mode)
      (chimera--hydra-set-flag hydra :exiting))))

(provide 'chimera-hydra)
