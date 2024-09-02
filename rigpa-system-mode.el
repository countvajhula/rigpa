;;; rigpa-system-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to the system, or operating environment
;;

;;; Code:

(require 'evil)
(require 'lithium)
(require 'chimera)

(evil-define-state system
  "System state."
  :tag " <S> "
  :message "-- SYSTEM --")

(defun rigpa-system-battery-life ()
  "Show power info including battery life
   (Mac-specific, at the moment)."
  (interactive)
  (display-message-or-buffer (shell-command-to-string "pmset -g batt")))

(lithium-define-global-mode rigpa-system-mode
  "System mode"
  (("b" rigpa-system-battery-life t)
   ("s-i" rigpa-system-battery-life t)
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " system"
  :group 'rigpa)

(defun rigpa--on-system-mode-entry ()
  "Actions to take upon entering system mode."
  (evil-system-state))

(defun rigpa--on-system-mode-post-exit ()
  "Actions to take upon exiting system mode."
  (rigpa--enter-appropriate-mode))

(defvar chimera-system-mode
  (make-chimera-mode :name "system"
                     :enter #'rigpa-system-mode-enter
                     :exit #'rigpa-system-mode-exit
                     :pre-entry-hook 'rigpa-system-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-system-mode-post-exit-hook
                     :entry-hook 'rigpa-system-mode-post-entry-hook
                     :exit-hook 'rigpa-system-mode-pre-exit-hook
                     :manage-hooks nil))


(provide 'rigpa-system-mode)
;;; rigpa-system-mode.el ends here
