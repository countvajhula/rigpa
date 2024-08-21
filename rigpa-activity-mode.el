;;; rigpa-activity-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to your editing activity
;;

;;; Code:

(require 'evil)
(require 'lithium)
(require 'chimera)

(evil-define-state activity
  "Activity state."
  :tag " <A> "
  :message "-- ACTIVITY --")

(setq rigpa-activity-accumulate-buffer-name "MY-CLIPBOARD")

(defun rigpa-activity-yank-and-accumulate ()
  "'Yank'/accumulate text in a temporary buffer."
  (interactive)
  (unless (get-buffer rigpa-activity-accumulate-buffer-name)
    (rigpa-buffer-create rigpa-activity-accumulate-buffer-name))
  (with-current-buffer rigpa-activity-accumulate-buffer-name
    (goto-char (point-max)))
  (append-to-buffer rigpa-activity-accumulate-buffer-name
                    (if (region-active-p)
                        (region-beginning)
                      (line-beginning-position))
                    (if (region-active-p)
                        (region-end)
                      (line-end-position)))
  (with-current-buffer rigpa-activity-accumulate-buffer-name
    (goto-char (point-max))
    (insert "\n"))
  (deactivate-mark))

(defun rigpa-activity-paste-and-clear ()
  "Paste contents of paste buffer and empty it."
  (interactive)
  (insert-buffer rigpa-activity-accumulate-buffer-name)
  (kill-buffer rigpa-activity-accumulate-buffer-name))

;; TODO: only stop at points that change the buffer location
;; in a "significant" way. either by number of lines, or
;; by whether it's a different top-level definition, or whether
;; there are blank lines between original/current point and
;; the change location. That way, navigating will always be
;; useful and we don't need to move multiple times before
;; there is a useful change.
(defun rigpa-activity-previous ()
  "docstring"
  (interactive)
  (call-interactively 'goto-last-change))

(defun rigpa-activity-next ()
  "docstring"
  (interactive)
  (when (eq last-command 'rigpa-activity-previous)
    ;; emacs only cycles "forwards" through the change list
    ;; if the previous command was a "backwards" navigation
    ;; through this list. Since we're wrapping the internal
    ;; commands, we need to manually indicate that the last
    ;; command was equivalent to the internal backwards
    ;; changelist navigation command
    (setq last-command 'goto-last-change))
  (call-interactively 'goto-last-change-reverse))

(lithium-define-global-mode rigpa-activity-mode
  "Activity mode"
  (("h" rigpa-activity-previous)
   ("C-j" evil-jump-backward)
   ("C-k" evil-jump-forward)
   ("l" rigpa-activity-next)
   ("m" evil-set-marker)
   ("g" evil-goto-mark)
   ("y" rigpa-activity-yank-and-accumulate)
   ("p" rigpa-activity-paste-and-clear)
   ("a" rigpa-activity-previous t)
   ("s-a" rigpa-activity-previous t)
   ("i" ignore t)
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " activity"
  :group 'rigpa)

(defun rigpa--on-activity-mode-entry ()
  "Actions to take upon entering activity mode."
  (evil-activity-state))

(defun rigpa--on-activity-mode-post-exit ()
  "Actions to take upon exiting activity mode."
  (rigpa--enter-appropriate-mode))

(defun rigpa-enter-activity-mode ()
  "Enter activity mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `exit' in the lithium mode-defining macro."
  (lithium-enter-mode 'rigpa-activity-mode))

(defun rigpa-exit-activity-mode ()
  "Exit activity mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `enter' in the lithium mode-defining macro."
  (lithium-exit-mode 'rigpa-activity-mode))

(defvar chimera-activity-mode
  (make-chimera-mode :name "activity"
                     :enter #'rigpa-enter-activity-mode
                     :exit #'rigpa-exit-activity-mode
                     :pre-entry-hook 'rigpa-activity-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-activity-mode-post-exit-hook
                     :entry-hook 'rigpa-activity-mode-post-entry-hook
                     :exit-hook 'rigpa-activity-mode-pre-exit-hook
                     :manage-hooks nil))


(provide 'rigpa-activity-mode)
;;; rigpa-activity-mode.el ends here
