
;; move these to epistemic.el?
(defun eem-hydra-flag-mode-exit (mode &optional value)
  "Set a mode exit flag to indicate cleanup operations need to be performed."
  (message "updating flag: %s %s" mode value)
  (let ((hydra (intern (concat "hydra-" mode))))
    (hydra-set-property hydra :exiting value)))

(defun eem-hydra-signal-exit (mode callback)
  "Helper function to witness hydra exit and notify epistemic mode."
  (let ((hydra (intern (concat "hydra-" mode))))
    (when (hydra-get-property hydra :exiting)
      (funcall callback mode)
      (hydra-set-property hydra :exiting nil))))

(provide 'eem-mode-adapter-hydra)
