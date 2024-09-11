;;; rigpa-history-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to the file's history of changes
;;

;;; Code:

(require 'evil)
(require 'lithium)
(require 'chimera)
(require 'git-timemachine)

(evil-define-state history
  "History state."
  :tag " <C> "
  :message "-- x→o --")

(lithium-define-global-mode rigpa-history-mode
  "History mode"
  (("h" git-timemachine-show-previous-revision)
   ("l" git-timemachine-show-next-revision)
   ("M-l" git-timemachine-show-current-revision)
   ("b" git-timemachine-blame t)
   ("/" git-timemachine-show-revision-fuzzy)
   ("?" git-timemachine-show-commit)
   ("q" git-timemachine-quit t)
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " history"
  :group 'rigpa)

(defun rigpa--on-history-mode-pre-entry ()
  "Pre-entry"
  (unless git-timemachine-mode
    (git-timemachine)))

(defun rigpa--on-history-mode-entry ()
  "Actions to take upon entry into history mode."
  ;; TODO: probably do this via a standard internal
  ;; rigpa hook in mode registration
  (evil-history-state))

(defun rigpa--on-history-mode-post-exit ()
  "Actions to take upon exit from history mode."
  ;; TODO: probably do this via a standard internal
  ;; rigpa hook in mode registration
  (rigpa--enter-local-evil-state))

(defvar chimera-history-mode
  (make-chimera-mode :name "history"
                     :enter #'rigpa-history-mode-enter
                     :exit #'rigpa-history-mode-exit
                     :pre-entry-hook 'rigpa-history-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-history-mode-post-exit-hook
                     :entry-hook 'rigpa-history-mode-post-entry-hook
                     :exit-hook 'rigpa-history-mode-pre-exit-hook
                     :manage-hooks nil))


(provide 'rigpa-history-mode)
;;; rigpa-history-mode.el ends here
