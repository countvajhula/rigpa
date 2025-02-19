;;; rigpa-symex-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to symbolic expressions (provided by separate package)
;;

;;; Code:

(require 'symex)
(require 'chimera)
(require 'evil)

(evil-define-state symex
  "Symex state."
  :tag " <λ> "
  :message "-- SYMEX --")

(defvar chimera-symex-mode
  (make-chimera-mode :name "symex"
                     :enter #'symex-mode-interface
                     :exit #'symex-editing-mode-exit
                     :pre-entry-hook 'symex-editing-mode-pre-entry-hook
                     :post-exit-hook 'symex-editing-mode-post-exit-hook
                     :entry-hook 'symex-editing-mode-post-entry-hook
                     :exit-hook 'symex-editing-mode-pre-exit-hook
                     :manage-hooks nil))

(defun rigpa--on-symex-mode-entry ()
  "Actions to take upon entering symex mode."
  (evil-symex-state))

(defun rigpa--on-symex-mode-post-exit ()
  "Actions to take upon exiting line mode."
  ;; TODO: return to tower instead. See
  ;; comment on line mode post exit
  (rigpa--enter-local-evil-state))

(provide 'rigpa-symex-mode)
;;; rigpa-symex-mode.el ends here
