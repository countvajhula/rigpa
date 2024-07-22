;;; chimera-hydra.el --- Self-reflective editing modes -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/rigpa

;; This program is "part of the world," in the sense described at
;; http://drym.org.  From your perspective, this is no different than
;; MIT or BSD or other such "liberal" licenses that you may be
;; familiar with, that is to say, you are free to do whatever you like
;; with this program.  It is much more than BSD or MIT, however, in
;; that it isn't a license at all but an idea about the world and how
;; economic systems could be set up so that everyone wins.  Learn more
;; at drym.org.
;;
;; This work transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.
;;

;;; Commentary:
;;
;; Hydra adapter for the chimera modal interface abstraction layer
;;

;;; Code:

(require 'evil)
(require 'hydra)
(require 'chimera)

(defun chimera--hydra-for-state (mode-name)
  (intern (concat "hydra-" mode-name)))

(defun chimera--hydra-set-flag (hydra flag &optional value)
  "Set a FLAG on the HYDRA with the value VALUE.

If no VALUE is provided, this clears the flag."
  (hydra-set-property hydra flag value))

(defun chimera-hydra-is-active-p (name)
  "Check whether the hydra named NAME is active."
  (let ((hydra-keymap (symbol-value (intern (concat "hydra-" name "/keymap")))))
    (eq hydra-curr-map hydra-keymap)))

(defun chimera-hydra-portend-exit (mode &optional value)
  "Set a mode exit flag to indicate cleanup operations need to be performed."
  (let* ((mode-name (chimera-mode-name mode))
         (hydra (chimera--hydra-for-state mode-name)))
    (chimera--hydra-set-flag hydra :exiting value)))

(defun chimera-hydra-signal-exit (mode callback)
  "Helper function to witness hydra exit and notify rigpa."
  (let* ((mode-name (chimera-mode-name mode))
         (hydra (chimera--hydra-for-state mode-name)))
    (when (hydra-get-property hydra :exiting)
      ;; TODO: probably don't need a generic callback here
      ;; rather, we should invoke handle-exit directly,
      ;; forwarding any callback(s) to it
      (funcall callback mode)
      (chimera--hydra-set-flag hydra :exiting))))

(defun chimera-hydra-signal-entry (mode)
  "Helper to witness hydra entry."
  (let* ((mode-name (chimera-mode-name mode))
         (hydra (chimera--hydra-for-state mode-name)))
    (chimera--hydra-set-flag hydra :entry-buffer (current-buffer))))

(defun chimera-handle-hydra-exit (mode)
  "Adapter helper for hydra to call hooks upon exit."
  (let ((mode-name (chimera-mode-name mode)))
    (when (chimera-mode-manage-hooks mode)
      (run-hooks (chimera-mode-exit-hook mode)))
    ;; TODO: chimera mustn't know about rigpa, so we should operate in
    ;; terms of callbacks here rather than call the rigpa interfaces
    ;; directly
    ;; TODO: there does not seem to be a way to use this to
    ;; identify post-exit in the case where a hydra is exited through
    ;; a non-head, since after-exit isn't called in that case ¯\_(ツ)_/¯
    ;; .. unless we can detect that a foreign key has been pressed in
    ;; the post hook, since that _is_ called always including for
    ;; foreign keys (but, alas, before executing the command).
    ;; In any case, this means that exit through non-head is not
    ;; a clean exit at this point.
    (when (equal (symbol-name evil-state) mode-name)
      ;; hydra has exited but we haven't gone to a new state.
      ;; This means limbo, and we need to enter an appropriate
      ;; state for the buffer here
      ;; although, should we do nothing if current mode is
      ;; already in the tower?
      ;; [doing this for now to fix symex margins issue, but
      ;; not sure exactly what is happening there]
      (unless (rigpa-ensemble-member-position-by-name (rigpa--local-tower)
                                                      (symbol-name evil-state))
        (rigpa--enter-appropriate-mode)))
    (let* ((hydra (chimera--hydra-for-state mode-name))
           (entry-buffer (hydra-get-property hydra :entry-buffer)))
      (when (and entry-buffer (buffer-live-p entry-buffer))
        ;; ensure the entry buffer reverts to a sane state
        (rigpa--enter-appropriate-mode entry-buffer))
      (chimera--hydra-set-flag hydra :entry-buffer))
    (run-hooks (chimera-mode-post-exit-hook mode))))

(provide 'chimera-hydra)
;;; chimera-hydra.el ends here
