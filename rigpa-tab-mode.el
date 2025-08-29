;;; rigpa-tab-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to tabs
;;

;;; Code:

(require 'evil)
(require 'chimera)
(require 'lithium)
(require 'centaur-tabs)

(defun rigpa-tab-setup-marks-table ()
  "Initialize the tab marks hashtable and add an entry for the
current ('original') tab."
  (interactive)
  (defvar rigpa-tab-marks-hash
    (make-hash-table :test 'equal))
  (rigpa-tab-save 'original))

(defun rigpa-tab-save (key)
  "Save current tab as original tab."
  (interactive)
  (puthash key (current-buffer)
           rigpa-tab-marks-hash))

(defun rigpa-tab-load (key)
  "Return to the buffer we were in at the time of entering
buffer mode."
  (interactive)
  (switch-to-buffer
   (gethash key rigpa-tab-marks-hash)))

(defun rigpa-tab-flash-to-original ()
  "Go momentarily to original tab and return.

This 'flash' allows the original tab, rather than the previous one
encountered while navigating to the present one, to be treated as the
last tab for 'flashback' ('Alt-tab') purposes. The flash should
happen quickly enough not to be noticeable."
  (interactive)
  (unless (equal (current-buffer) (rigpa-tab-original))
    (let ((inhibit-redisplay t)) ;; not sure if this is doing anything but FWIW
      (rigpa-tab-load 'original)
      (rigpa-tab-save 'previous)
      (evil-switch-to-windows-last-buffer))))

(defun rigpa-tab-original ()
  "Get original tab identifier"
  (interactive)
  (gethash 'original rigpa-tab-marks-hash))

(defun rigpa-tab-return-to-original ()
  "Return to the tab we were in at the time of entering
buffer mode."
  (interactive)
  (rigpa-tab-load 'original))

(defun rigpa-tab-switch-to-last ()
  (interactive)
  (rigpa-tab-save 'temp-previous)
  (rigpa-tab-load 'previous)
  (puthash 'previous (gethash 'temp-previous rigpa-tab-marks-hash)
           rigpa-tab-marks-hash))

(lithium-define-global-mode rigpa-tab-mode
  "Tab mode"
  (("/" centaur-tabs-counsel-switch-group t)
   ("h" centaur-tabs-backward)
   ("s-{" centaur-tabs-backward) ; to "take over" from the global binding
   ("l" centaur-tabs-forward)
   ("s-}" centaur-tabs-forward) ; to "take over" from the global binding
   ("k" (lambda ()
          (interactive)
          (centaur-tabs-backward-group)))
   ("j" (lambda ()
          (interactive)
          (centaur-tabs-forward-group)))
   ("H" centaur-tabs-move-current-tab-to-left)
   ("L" centaur-tabs-move-current-tab-to-right)
   ("s-t" rigpa-tab-switch-to-last t)
   ("t" rigpa-tab-switch-to-last t)
   ("n" (lambda ()
          (interactive)
          (rigpa-buffer-create nil nil :switch-p t))
    t)
   ("x" kill-buffer)
   ("?" rigpa-buffer-info t)
   ("q" rigpa-tab-return-to-original t)
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " tab"
  :group 'rigpa)

(defun rigpa--on-tab-mode-entry ()
  "Actions to take upon entry into tab mode."
  (rigpa-tab-setup-marks-table))

(defun rigpa--on-tab-mode-post-exit ()
  "Actions to take upon exiting tab mode."
  (rigpa-tab-flash-to-original))

(defvar chimera-tab-mode
  (make-chimera-mode :name "tab"
                     :enter #'rigpa-tab-mode-enter
                     :exit #'rigpa-tab-mode-exit
                     :pre-entry-hook 'rigpa-tab-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-tab-mode-post-exit-hook
                     :entry-hook 'rigpa-tab-mode-post-entry-hook
                     :exit-hook 'rigpa-tab-mode-pre-exit-hook
                     :manage-hooks nil))

(defun rigpa-tab-initialize ()
  "Initialize Tab mode."
  (add-hook 'rigpa-tab-mode-post-entry-hook
            #'rigpa--on-tab-mode-entry))


(provide 'rigpa-tab-mode)
;;; rigpa-tab-mode.el ends here
