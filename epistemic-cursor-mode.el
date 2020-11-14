(cl-defstruct epistemic-cursor-mode
  "Specification for a mode."
  enter
  (exit nil)
  (entry-hook nil)
  (exit-hook nil))

(defun eem-register-mode (mode-name)
  "Register MODE-NAME for use with epistemic mode."
  (let* ((mode (symbol-value (intern (concat "epistemic-" mode-name "-mode"))))
         (entry-hook (epistemic-cursor-mode-entry-hook mode))
         (exit-hook (epistemic-cursor-mode-exit-hook mode)))
    (add-hook entry-hook
                #'eem-jump-to-level-too-after)))

;; TODO: do this formally in the right place, for all modes registered
;; esp. write mode abstractions for and register built-in evil states
(add-hook 'evil-insert-state-entry-hook
            #'eem-jump-to-level-too-before)

;; (defun eem-unregister-mode (mode-name)
;;   "Unregister MODE-NAME."
;;   (let* ((mode (symbol-value (intern (concat "epistemic-" mode-name "-mode"))))
;;          (entry-hook (epistemic-cursor-mode-entry-hook mode))
;;          (exit-hook (epistemic-cursor-mode-exit-hook mode)))
;;     (remove-hook exit-hook #'eem-jump-to-level-too)))

(provide 'epistemic-cursor-mode)
;;; epistemic-cursor-mode.el ends here
