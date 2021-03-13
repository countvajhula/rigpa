(require 'rigpa-types)

(defun rigpa--get-ground-buffer ()
  "Get the ground buffer.

At the lowest level, the buffer is expected to refer to itself
to terminate the reference chain."
  (unless rigpa--ground-buffer
    (setq rigpa--ground-buffer (current-buffer)))
  rigpa--ground-buffer)

(defun rigpa--buffer-name (ensemble)
  "Buffer name to use for a given ensemble."
  (concat rigpa-buffer-prefix "-" (rigpa-editing-entity-name ensemble)))

(defun rigpa--set-meta-buffer-appearance ()
  "Configure meta mode buffer appearance."
  (buffer-face-set 'rigpa-face)
  (text-scale-set 5)
  ;;(setq cursor-type nil))
  (internal-show-cursor nil nil)
  (when display-line-numbers-mode
    (display-line-numbers-mode -1))
  (hl-line-mode))

(defun rigpa--set-ui-for-meta-modes ()
  "Set (for now, global) UI parameters for meta modes."
  ;; should ideally be perspective-specific
  (blink-cursor-mode -1))

(defun rigpa--revert-ui ()
  "Revert buffer appearance to settings prior to entering mode mode."
  (blink-cursor-mode 1))


(provide 'rigpa-meta)
;;; rigpa-meta.el ends here
