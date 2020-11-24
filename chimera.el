(cl-defstruct chimera-mode
  "Specification for a mode."
  name
  (enter nil :documentation "Primitive mode entry function.") ; this is required
  (exit nil :documentation "Primitive mode exit function.") ; we don't need to rely on exit being defined
  (entry-hook nil)
  (exit-hook nil)
  (manage-hooks nil
                :documentation "Whether hooks should be managed internally. \
If not, they are expected to be run by the underlying mode provider \
(e.g. evil or hydra)."))

(defvar chimera-evil-states
  (list "normal" "insert" "emacs" "visual" "replace"))

(defun chimera-enter-mode (mode)
  "Enter MODE."
  (interactive)
  (let ((name (chimera-mode-name mode)))
    (message "entering mode %s" name)
    ;; call a function (perform-entry-actions ...) that
    ;; handles any provider-specific jankiness, like checking
    ;; for hydras that didn't exit cleanly, and perform their
    ;; exit actions (which should be in a dedicated function
    ;; that can be called from here as well as the original
    ;; spot in the hydra exit lifecycle phase).
    (unless (member name chimera-evil-states)
      (let ((evil-state-entry (intern (concat "evil-" name "-state"))))
        (funcall evil-state-entry)
        (message "changed to %s state" name)))
    (funcall (chimera-mode-enter mode))
    (when (chimera-mode-manage-hooks mode)
      ;; for now, we rely on evil hooks for all modes (incl.
      ;; hydra-based ones), and this should never be called.
      (message "Running entry hooks for %s mode" name)
      (run-hooks (chimera-mode-entry-hook mode)))
    (message "entered mode %s" name)))

(defun chimera-exit-mode (mode)
  "Exit (interrupt) MODE."
  (interactive)
  (funcall (chimera-mode-exit mode)))


(provide 'chimera)
;;; chimera.el ends here
