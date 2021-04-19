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

(require 'chimera)
(require 'chimera-hydra)

(evil-define-state text
  "Text state."
  :tag " <A> "
  :message "-- TEXT --"
  :enable (normal))


(defhydra hydra-text (:columns 2
                      :body-pre (chimera-hydra-signal-entry chimera-text-mode)
                      :post (chimera-hydra-portend-exit chimera-text-mode t)
                      :after-exit (chimera-hydra-signal-exit chimera-text-mode
                                                             #'chimera-handle-hydra-exit))
  "Text mode"
  ("z" evil-fill-and-move "justify" :exit t)
  ("s-z" evil-fill-and-move "justify" :exit t)
  ("i" nil "exit" :exit t)
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-text-mode-entry-hook nil
  "Entry hook for rigpa text mode.")

(defvar chimera-text-mode-exit-hook nil
  "Exit hook for rigpa text mode.")

(defvar chimera-text-mode
  (make-chimera-mode :name "text"
                     :enter #'hydra-text/body
                     :pre-entry-hook 'chimera-text-mode-entry-hook
                     :post-exit-hook 'chimera-text-mode-exit-hook
                     :entry-hook 'evil-text-state-entry-hook
                     :exit-hook 'evil-text-state-exit-hook))


(provide 'rigpa-text-mode)
;;; rigpa-text-mode.el ends here
