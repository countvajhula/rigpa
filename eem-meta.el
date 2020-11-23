(require 'eem-types)

(defun eem--get-ground-buffer ()
  "Get the ground buffer.

At the lowest level, the buffer is expected to refer to itself
to terminate the reference chain."
  (unless eem--ground-buffer
    (setq eem--ground-buffer (current-buffer)))
  eem--ground-buffer)

(defun eem--buffer-name (ensemble)
  "Buffer name to use for a given ensemble."
  (concat eem-buffer-prefix "-" (eem-editing-entity-name ensemble)))

(defun eem--set-meta-buffer-appearance ()
  "Configure meta mode buffer appearance."
  (buffer-face-set 'eem-face)
  (text-scale-set 5)
  ;;(setq cursor-type nil))
  (internal-show-cursor nil nil)
  (display-line-numbers-mode 'toggle)
  (hl-line-mode))

(defun eem--set-ui-for-meta-modes ()
  "Set (for now, global) UI parameters for meta modes."
  ;; should ideally be perspective-specific
  (blink-cursor-mode -1))

(defun eem--revert-ui ()
  "Revert buffer appearance to settings prior to entering mode mode."
  (blink-cursor-mode 1))


(provide 'eem-meta)
;;; eem-meta.el ends here
