;;; rigpa-window-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to windows
;;

;;; Code:

(require 'evil)
(require 'ace-window)
(require 'transpose-frame)
(require 'winner)
(require 'windmove)
(require 'chimera)
(require 'lithium)
(require 'rigpa-util)
(require 'dash)

(evil-define-state window
  "Window state."
  :tag " <W> "
  :message "-- WINDOW --")

;; configure home-row hotkeys to index windows in ace-window,
;; used as "search" feature in window mode
(setq aw-keys '(?h ?j ?k ?l ?g ?f ?d ?s ?a))

;; enable winner mode, used to provide "undo/redo" functionality
;; in window mode
(winner-mode t)

;; enable fitting window to buffer width
(setq fit-window-to-buffer-horizontally t)

(defun rigpa-window-mru ()
  "Jump to most recent window, or other window if there is only one other.

TODO: This doesn't work with more than 2 windows that are all the same buffer."
  (interactive)
  (let ((num-windows (length (window-list))))
    (if (= num-windows 2)
        (other-window 1)
      (evil-window-mru))))

(defun rigpa-window-quit-other ()
  "Quit other window without changing focus."
  (interactive)
  (let ((original-window (selected-window)))
    (other-window 1)
    (quit-window)
    (select-window original-window)))

(defun rigpa-window--opposite-direction (direction)
  "The opposite direction."
  (cond ((eq 'left direction) 'right)
        ((eq 'right direction) 'left)
        ((eq 'up direction) 'down)
        ((eq 'down direction) 'up)))

(defun rigpa-window--find (direction)
  "Find window in DIRECTION."
  (let ((original-window (selected-window))
        (next-window (windmove-find-other-window direction)))
    (cond ((save-window-excursion
             (rigpa-window-mru)
             (let ((window (windmove-find-other-window
                            (rigpa-window--opposite-direction direction))))
               (and window
                    (eq original-window window))))
           (save-window-excursion (rigpa-window-mru)
                                  (selected-window)))
          ((and next-window
                (not (window-minibuffer-p next-window)))
           next-window)
          (t nil))))

(defun rigpa-window--go (direction)
  "Select window in DIRECTION."
  (let ((window (rigpa-window--find direction)))
    (when window
      (select-window window))))

(defun rigpa-window-left ()
  "Select window on the left."
  (interactive)
  (rigpa-window--go 'left))

(defun rigpa-window-right ()
  "Select window on the right."
  (interactive)
  (rigpa-window--go 'right))

(defun rigpa-window-up ()
  "Select window above."
  (interactive)
  (rigpa-window--go 'up))

(defun rigpa-window-down ()
  "Select window below."
  (interactive)
  (rigpa-window--go 'down))

(defun rigpa-window--move-buffer (direction)
  "Move buffer in current window in DIRECTION.

If both buffers are the same, then just preserve the position in the
buffer from the source context."
  (let ((buffer (current-buffer))
        (original-position (point))
        (next-window (rigpa-window--find direction)))
    (when next-window
      (switch-to-buffer (other-buffer))
      (select-window next-window)
      (unless (eq buffer (current-buffer))
          (switch-to-buffer buffer))
      (goto-char original-position)
      (recenter))))

(defun rigpa-window-move-buffer-left ()
  "Move buffer in current window to the window on the left."
  (interactive)
  (rigpa-window--move-buffer 'left))

(defun rigpa-window-move-buffer-right ()
  "Move buffer in current window to the window on the right."
  (interactive)
  (rigpa-window--move-buffer 'right))

(defun rigpa-window-move-buffer-up ()
  "Move buffer in current window to the window above."
  (interactive)
  (rigpa-window--move-buffer 'up))

(defun rigpa-window-move-buffer-down ()
  "Move buffer in current window to the window below."
  (interactive)
  (rigpa-window--move-buffer 'down))

(defun rigpa-window--exchange-buffer (direction)
  "Exchange buffer with the window on the DIRECTION side.

If both windows contain the same buffer, simply exchange cursor
positions."
  (interactive)
  (let ((buffer (current-buffer))
        (original-position (point))
        (original-window (selected-window))
        (next-window (rigpa-window--find direction)))
    (when next-window
      (select-window next-window)
      (let ((other-position (point))
            (other-buffer (current-buffer)))
        (unless (eq buffer other-buffer)
          (switch-to-buffer buffer))
        (goto-char original-position)
        (recenter)
        (select-window original-window)
        (unless (eq buffer other-buffer)
          (switch-to-buffer other-buffer))
        (goto-char other-position)
        (recenter)
        (select-window next-window)))))

(defun rigpa-window-exchange-buffer-left ()
  "Exchange buffer with the one on the left."
  (interactive)
  (rigpa-window--exchange-buffer 'left))

(defun rigpa-window-exchange-buffer-right ()
  "Exchange buffer with the one on the right."
  (interactive)
  (rigpa-window--exchange-buffer 'right))

(defun rigpa-window-exchange-buffer-up ()
  "Exchange buffer with the one on the up."
  (interactive)
  (rigpa-window--exchange-buffer 'up))

(defun rigpa-window-exchange-buffer-down ()
  "Exchange buffer with the one on the down."
  (interactive)
  (rigpa-window--exchange-buffer 'down))

(defun rigpa-window-setup-marks-table ()
  "Initialize the buffer marks hashtable and add an entry for the
current ('original') buffer."
  (interactive)
  (defvar rigpa-window-marks-hash
    (make-hash-table :test 'equal))
  (rigpa-window-save-original))

(defun rigpa-window-save-original ()
  "Save current buffer as original buffer."
  (interactive)
  (rigpa-window-set-mark ?0))

(defun rigpa-window-original-configuration ()
  "Get original window configuration."
  (interactive)
  (cdr (rigpa-window-get-mark ?0)))

(defun rigpa-window-original-window ()
  "Get original selected window."
  (interactive)
  (car (rigpa-window-get-mark ?0)))

(defun rigpa-window-set-mark (mark-name)
  "Set a mark"
  (interactive "cMark name?")
  (puthash mark-name
           (cons (selected-window)
                 (winner-configuration))
           rigpa-window-marks-hash)
  (message "Mark '%c' set." mark-name))

(defun rigpa-window-get-mark (mark-name)
  "Retrieve a mark"
  (gethash mark-name rigpa-window-marks-hash))

(defun rigpa-window-return-to-mark (mark-name)
  "Return to mark"
  (interactive "cMark name?")
  (winner-set (cdr (rigpa-window-get-mark mark-name))))

(defun rigpa-window-return-to-original-configuration ()
  "Return to the window configuration we were in at the time of entering
window mode."
  (interactive)
  (winner-set (rigpa-window-original-configuration)))

(defun rigpa-window-return-to-original-window ()
  "Return to the window we were in at the time of entering
window mode."
  (interactive)
  (let ((window (rigpa-window-original-window)))
    ;; TODO: it seems that when a window (e.g. REPL) is closed
    ;; it shows Normal mode in the other (only remaining) window
    ;; but it doesn't allow you to move, since Window mode is
    ;; still invisibly active.
    ;; FIX: for global modes like Window, probably make them
    ;; "globalized" so that they they set the correct buffer-local
    ;; evil state
    (when (window-live-p window)
      (select-window window))))

(defun rigpa-window-flash-to-original ()
  "Go momentarily to original window and return.

This 'flash' allows the original window, rather than the previous one
encountered while navigating to the present one, to be treated as the
last window for 'flashback' ('Alt-tab') purposes. The flash should
happen quickly enough not to be noticeable."
  (interactive)
  ;; TODO: in some cases when a REPL window (maybe large > 2500 lines) is open
  ;; this doesn't seem to restore the target window after the "excursion"
  (unless (eq (selected-window) (rigpa-window-original-window))
    (let ((inhibit-redisplay t)) ;; not sure if this is doing anything but FWIW
      (save-window-excursion
        (rigpa-window-return-to-original-window)))))

(lithium-define-global-mode rigpa-window-mode
  "Window mode"
  (("h" rigpa-window-left)
   ("j" rigpa-window-down)
   ("k" rigpa-window-up)
   ("l" rigpa-window-right)
   ("H" rigpa-window-move-buffer-left)
   ("J" rigpa-window-move-buffer-down)
   ("K" rigpa-window-move-buffer-up)
   ("L" rigpa-window-move-buffer-right)
   ("C-S-h" rigpa-window-exchange-buffer-left)
   ("C-S-j" rigpa-window-exchange-buffer-down)
   ("C-S-k" rigpa-window-exchange-buffer-up)
   ("C-S-l" rigpa-window-exchange-buffer-right)
   ("M-H" evil-window-move-far-left)
   ("M-J" evil-window-move-very-bottom)
   ("M-K" evil-window-move-very-top)
   ("M-L" evil-window-move-far-right)
   ("x" evil-window-delete)
   ("d" evil-window-delete t)
   ("X" transpose-frame) ; there are more in transpose-frame that may be useful
   ("Q" rigpa-window-quit-other t)
   ("o" rigpa-window-mru t)
   ("s-w" rigpa-window-mru t)
   ("n" other-window)
   ("w" delete-other-windows t)
   ("s" evil-window-split)
   ("_" evil-window-split)
   ("v" evil-window-vsplit)
   ("|" evil-window-vsplit)
   ("u" winner-undo)
   ("C-r" winner-redo)
   ("m" rigpa-window-set-mark)
   ("'" rigpa-window-return-to-mark t)
   ("`" rigpa-window-return-to-mark t)
   ("q" rigpa-window-return-to-original-configuration t)
   ("/" ace-window)
   ("+" evil-window-increase-height)
   ("-" evil-window-decrease-height)
   ("C-+" (lambda ()
            (interactive)
            (evil-window-increase-height 3)))
   ("C--" (lambda ()
            (interactive)
            (evil-window-decrease-height 3)))
   (">" evil-window-increase-width)
   ("<" evil-window-decrease-width)
   ("C->" (lambda ()
            (interactive)
            (evil-window-increase-width 5)))
   ("C-<" (lambda ()
            (interactive)
            (evil-window-decrease-width 5)))
   ("<backspace>" balance-windows)
   ("<tab>" fit-window-to-buffer)
   ("=" fit-window-to-buffer)
   ("r" evil-window-rotate-downwards)
   ("R" evil-window-rotate-upwards)
   ("f" ffap-other-window t)
   ("i" nil t)
   ("<return>" rigpa-enter-lower-level t)
   ("<escape>" rigpa-enter-higher-level t))
  :lighter " window"
  :group 'rigpa)

(defun rigpa--on-window-mode-entry ()
  "Actions to take upon entry into window mode."
  (rigpa-window-setup-marks-table)
  ;; TODO: if it is already active, then make a note
  ;; of it and don't disable it upon exit
  (auto-dim-other-buffers-mode 1)
  ;; TODO: probably do this via a standard internal
  ;; rigpa hook in mode registration
  (rigpa--for-all-buffers #'evil-window-state))

(defun rigpa--on-window-mode-post-exit ()
  "Actions to take upon exit from window mode."
  (rigpa-window-flash-to-original)
  (auto-dim-other-buffers-mode -1)
  ;; TODO: probably do this via a standard internal
  ;; rigpa hook in mode registration
  (rigpa--for-all-buffers #'rigpa--enter-appropriate-mode))

(defun rigpa-enter-window-mode ()
  "Enter window mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `exit' in the lithium mode-defining macro."
  (lithium-enter-mode 'rigpa-window-mode))

(defun rigpa-exit-window-mode ()
  "Exit window mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `enter' in the lithium mode-defining macro."
  (lithium-exit-mode 'rigpa-window-mode))

(defvar chimera-window-mode
  (make-chimera-mode :name "window"
                     :enter #'rigpa-enter-window-mode
                     :exit #'rigpa-exit-window-mode
                     :pre-entry-hook 'rigpa-window-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-window-mode-post-exit-hook
                     :entry-hook 'rigpa-window-mode-post-entry-hook
                     :exit-hook 'rigpa-window-mode-pre-exit-hook
                     :manage-hooks nil))


(provide 'rigpa-window-mode)
;;; rigpa-window-mode.el ends here
