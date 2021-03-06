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

(defvar chimera-symex-mode-entry-hook nil
  "Entry hook for rigpa symex mode.")

(defvar chimera-symex-mode-exit-hook nil
  "Exit hook for rigpa symex mode.")

(defun rigpa--enable-symex-minor-mode ()
  "Enable symex minor mode."
  (symex-enable-editing-minor-mode))

(defun rigpa--disable-symex-minor-mode ()
  "Disable symex minor mode."
  (symex-disable-editing-minor-mode))

(defvar chimera-symex-mode
  (make-chimera-mode :name "symex"
                     :enter #'symex-mode-interface
                     :pre-entry-hook 'chimera-symex-mode-entry-hook
                     :post-exit-hook 'chimera-symex-mode-exit-hook
                     :entry-hook 'evil-symex-state-entry-hook
                     :exit-hook 'evil-symex-state-exit-hook))


(provide 'rigpa-symex-mode)
;;; rigpa-symex-mode.el ends here
