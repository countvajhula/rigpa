;;; rigpa-evil-support.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; Supporting utilities for evil-backed modes
;;

;;; Code:

(require 'evil)

(defun rigpa--define-evil-key (key fn map state)
  "Define an evil keybinding in an evil-backed rigpa mode."
  (evil-define-key (list state 'visual 'operator)
                   map
                   (kbd key)
                   fn))

(defun rigpa--define-evil-keys-from-spec (keyspec keymap state)
  "Define evil keys from a specification."
  (dolist (keybinding keyspec)
    (rigpa--define-evil-key (car keybinding)
                            (cdr keybinding)
                            keymap
                            state)))


(provide 'rigpa-evil-support)
;;; rigpa-evil-support.el ends here
