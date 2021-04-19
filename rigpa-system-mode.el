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

(require 'chimera)
(require 'chimera-hydra)

(evil-define-state system
  "System state."
  :tag " <S> "
  :message "-- SYSTEM --"
  :enable (normal))

(defun rigpa-system-battery-life ()
  "Show power info including battery life
   (Mac-specific, at the moment)."
  (interactive)
  (display-message-or-buffer (shell-command-to-string "pmset -g batt")))

(defhydra hydra-system (:exit t
                        :body-pre (chimera-hydra-signal-entry chimera-system-mode)
                        :post (chimera-hydra-portend-exit chimera-system-mode t)
                        :after-exit (chimera-hydra-signal-exit chimera-system-mode
                                                               #'chimera-handle-hydra-exit))
  "System information"
  ("b" rigpa-system-battery-life "show power info including battery life")
  ("s-i" rigpa-system-battery-life "show power info including battery life")
  ("H-m" rigpa-toggle-menu "show/hide this menu" :exit nil)
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-system-mode-entry-hook nil
  "Entry hook for rigpa system mode.")

(defvar chimera-system-mode-exit-hook nil
  "Exit hook for rigpa system mode.")

(defvar chimera-system-mode
  (make-chimera-mode :name "system"
                     :enter #'hydra-system/body
                     :pre-entry-hook 'chimera-system-mode-entry-hook
                     :post-exit-hook 'chimera-system-mode-exit-hook
                     :entry-hook 'evil-system-state-entry-hook
                     :exit-hook 'evil-system-state-exit-hook))


(provide 'rigpa-system-mode)
;;; rigpa-system-mode.el ends here
