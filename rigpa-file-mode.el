;;; rigpa-file-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to the open file
;;

;;; Code:

(require 'evil)
(require 'lithium)
(require 'chimera)

(evil-define-state file
  "File state."
  :tag " <F> "
  :message "-- FILE --")

;; From: https://www.emacswiki.org/emacs/MarkCommands#toc4
(defun unpop-to-mark-command ()
    "Unpop off mark ring. Does nothing if mark ring is empty."
    (interactive)
    (when mark-ring
      (let ((pos (marker-position (car (last mark-ring)))))
        (if (not (= (point) pos))
            (goto-char pos)
          (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
          (set-marker (mark-marker) pos)
          (setq mark-ring (nbutlast mark-ring))
          (goto-char (marker-position (car (last mark-ring))))))))

(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))

(defun rigpa-file-yank ()
  "Save current buffer contents."
  (interactive)
  (copy-to-register ?f (point-min) (point-max)))

(defun rigpa-file-paste ()
  "Paste saved buffer contents."
  (interactive)
  (insert-register ?f))

(lithium-define-global-mode rigpa-file-mode
  "File mode"
  (("h" evil-backward-char)
   ("j" evil-next-line)
   ("k" evil-previous-line)
   ("l" evil-forward-char)
   ("M-h" evil-goto-first-line)
   ("M-l" evil-goto-line)
   ("C-h" xah-pop-local-mark-ring)
   ("C-l" unpop-to-mark-command)
   ("y" rigpa-file-yank)
   ("p" rigpa-file-paste)
   ("i" nil t)
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " file"
  :group 'rigpa)

(defun rigpa--on-file-mode-entry ()
  "Actions to take upon entering file mode."
  (evil-file-state))

(defun rigpa--on-file-mode-post-exit ()
  "Actions to take upon exiting file mode."
  (rigpa--enter-appropriate-mode))

(defun rigpa-enter-file-mode ()
  "Enter file mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `exit' in the lithium mode-defining macro."
  (lithium-enter-mode 'rigpa-file-mode))

(defun rigpa-exit-file-mode ()
  "Exit file mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `enter' in the lithium mode-defining macro."
  (lithium-exit-mode 'rigpa-file-mode))

(defvar chimera-file-mode
  (make-chimera-mode :name "file"
                     :enter #'rigpa-enter-file-mode
                     :exit #'rigpa-exit-file-mode
                     :pre-entry-hook 'rigpa-file-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-file-mode-post-exit-hook
                     :entry-hook 'rigpa-file-mode-post-entry-hook
                     :exit-hook 'rigpa-file-mode-pre-exit-hook
                     :manage-hooks nil))


(provide 'rigpa-file-mode)
;;; rigpa-file-mode.el ends here
