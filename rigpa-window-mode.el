(require 'ace-window)
(require 'winner)
(require 'chimera)
(require 'chimera-hydra)

(evil-define-state window
  "Window state."
  :tag " <W> "
  :message "-- WINDOW --"
  :enable (normal))

;; configure home-row hotkeys to index windows in ace-window,
;; used as "search" feature in window mode
(setq aw-keys '(?h ?j ?k ?l ?g ?f ?d ?s ?a))

;; enable winner mode, used to provide "undo/redo" functionality
;; in window mode
(winner-mode t)

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
  (other-window 1)
  (quit-window))

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
  "Get original buffer identifier"
  (interactive)
  (rigpa-window-get-mark ?0))

(defun rigpa-window-set-mark (mark-name)
  "Set a mark"
  (interactive "cMark name?")
  (puthash mark-name (winner-configuration) rigpa-window-marks-hash)
  (message "Mark '%c' set." mark-name))

(defun rigpa-window-get-mark (mark-name)
  "Retrieve a mark"
  (gethash mark-name rigpa-window-marks-hash))

(defun rigpa-window-return-to-mark (mark-name)
  "Return to mark"
  (interactive "cMark name?")
  (winner-set (rigpa-window-get-mark mark-name)))

(defun rigpa-window-return-to-original ()
  "Return to the buffer we were in at the time of entering
buffer mode."
  (interactive)
  (winner-set (rigpa-window-original-configuration)))

(defhydra hydra-window (:columns 4
                        :body-pre (progn (rigpa-window-setup-marks-table)
                                         (chimera-hydra-signal-entry chimera-window-mode))
                        :post (chimera-hydra-portend-exit chimera-window-mode t)
                        :after-exit (chimera-hydra-signal-exit chimera-window-mode
                                                               #'chimera-handle-hydra-exit))
  "Window mode"
  ("h" rigpa-window-left "left")
  ("j" rigpa-window-down "down")
  ("k" rigpa-window-up "up")
  ("l" rigpa-window-right "right")
  ("H" rigpa-window-move-buffer-left "move buffer left")
  ("J" rigpa-window-move-buffer-down "move buffer down")
  ("K" rigpa-window-move-buffer-up "move buffer up")
  ("L" rigpa-window-move-buffer-right "move buffer right")
  ("C-S-h" rigpa-window-exchange-buffer-left "exchange buffer left")
  ("C-S-j" rigpa-window-exchange-buffer-down "exchange buffer down")
  ("C-S-k" rigpa-window-exchange-buffer-up "exchange buffer up")
  ("C-S-l" rigpa-window-exchange-buffer-right "exchange buffer left")
  ("M-H" evil-window-move-far-left "move to far left")
  ("M-J" evil-window-move-very-bottom "move to bottom")
  ("M-K" evil-window-move-very-top "move to top")
  ("M-L" evil-window-move-far-right "move to far right")
  ("x" evil-window-delete "delete")
  ("c" evil-window-delete)
  ("Q" rigpa-window-quit-other "quit other window" :exit t)
  ("o" rigpa-window-mru "Jump to most recent (like Alt-Tab)" :exit t)
  ("n" other-window "next")
  ("w" delete-other-windows "maximize" :exit t)
  ("s" evil-window-split "split horizontally")
  ("_" evil-window-split "")
  ("v" evil-window-vsplit "split vertically")
  ("|" evil-window-vsplit "")
  ("u" winner-undo "undo")
  ("C-r" winner-redo "redo")
  ("m" rigpa-window-set-mark "set mark")
  ("'" rigpa-window-return-to-mark "return to mark" :exit t)
  ("`" rigpa-window-return-to-mark "return to mark" :exit t)
  ("q" rigpa-window-return-to-original "return to original" :exit t)
  ("/" ace-window "search")
  ("+" evil-window-increase-height "expand vertically")
  ("-" evil-window-decrease-height "shrink vertically")
  (">" evil-window-increase-width "expand horizontally")
  ("<" evil-window-decrease-width "shrink horizontally")
  ("=" balance-windows "balance")
  ("r" evil-window-rotate-downwards "rotate downwards")
  ("R" evil-window-rotate-upwards "rotate upwards")
  ("f" ffap-other-window "go to file in other window" :exit t)
  ("i" nil "exit" :exit t)
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-window-mode-entry-hook nil
  "Entry hook for rigpa window mode.")

(defvar chimera-window-mode-exit-hook nil
  "Exit hook for rigpa window mode.")

(defvar chimera-window-mode
  (make-chimera-mode :name "window"
                     :enter #'hydra-window/body
                     :pre-entry-hook 'chimera-window-mode-entry-hook
                     :post-exit-hook 'chimera-window-mode-exit-hook
                     :entry-hook 'evil-window-state-entry-hook
                     :exit-hook 'evil-window-state-exit-hook))


(provide 'rigpa-window-mode)
;;; rigpa-window-mode.el ends here
