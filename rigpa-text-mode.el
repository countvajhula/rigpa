;;; rigpa-text-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to text
;;

;;; Code:

(require 'evil)
(require 'lithium)
(require 'chimera)

(evil-define-state text
  "Text state."
  :tag " <A> "
  :message "-- TEXT --")

(lithium-define-global-mode rigpa-text-mode
  "Text mode"
  (("z" evil-fill-and-move t)
   ("s-z" evil-fill-and-move t)
   ("i" nil t)
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " text"
  :group 'rigpa)

(defun rigpa--on-text-mode-entry ()
  "Actions to take upon entering text mode."
  (evil-text-state))

(defun rigpa--on-text-mode-post-exit ()
  "Actions to take upon exiting text mode."
  (rigpa--enter-appropriate-mode))

(defun rigpa-enter-text-mode ()
  "Enter text mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `exit' in the lithium mode-defining macro."
  (lithium-enter-mode 'rigpa-text-mode))

(defun rigpa-exit-text-mode ()
  "Exit text mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `enter' in the lithium mode-defining macro."
  (lithium-exit-mode 'rigpa-text-mode))

(defvar chimera-text-mode
  (make-chimera-mode :name "text"
                     :enter #'rigpa-enter-text-mode
                     :exit #'rigpa-exit-text-mode
                     :pre-entry-hook 'rigpa-text-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-text-mode-post-exit-hook
                     :entry-hook 'rigpa-text-mode-post-entry-hook
                     :exit-hook 'rigpa-text-mode-pre-exit-hook
                     :manage-hooks nil))


(provide 'rigpa-text-mode)
;;; rigpa-text-mode.el ends here
