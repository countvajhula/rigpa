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

(lithium-define-global-mode rigpa-text-mode
  "Text mode"
  (("z" evil-fill-and-move t)
   ("s-z" evil-fill-and-move t)
   ("i" nil t)
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " text"
  :group 'rigpa)

(defvar chimera-text-mode
  (make-chimera-mode :name "text"
                     :enter #'rigpa-text-mode-enter
                     :exit #'rigpa-text-mode-exit
                     :pre-entry-hook 'rigpa-text-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-text-mode-post-exit-hook
                     :entry-hook 'rigpa-text-mode-post-entry-hook
                     :exit-hook 'rigpa-text-mode-pre-exit-hook
                     :manage-hooks nil))

(defun rigpa-text-initialize ()
  "Initialize Text mode."
  nil)


(provide 'rigpa-text-mode)
;;; rigpa-text-mode.el ends here
