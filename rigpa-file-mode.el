(require 'chimera)
(require 'chimera-hydra)

(evil-define-state file
  "File state."
  :tag " <F> "
  :message "-- FILE --"
  :enable (normal))

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

(defhydra hydra-file (:columns 2
                      :post (chimera-hydra-portend-exit chimera-file-mode t)
                      :after-exit (chimera-hydra-signal-exit chimera-file-mode
                                                             #'chimera-handle-hydra-exit))
  "File mode"
  ("h" evil-backward-char "backward")
  ("j" evil-next-line "down")
  ("k" evil-previous-line "up")
  ("l" evil-forward-char "forward")
  ("M-h" evil-goto-first-line "beginning")
  ("M-l" evil-goto-line "end")
  ("C-h" xah-pop-local-mark-ring "previous mark")
  ("C-l" unpop-to-mark-command "next mark")
  ("y" rigpa-file-yank "yank")
  ("p" rigpa-file-paste "paste")
  ("i" nil "exit" :exit t)
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-file-mode-entry-hook nil
  "Entry hook for rigpa file mode.")

(defvar chimera-file-mode-exit-hook nil
  "Exit hook for rigpa file mode.")

(defvar chimera-file-mode
  (make-chimera-mode :name "file"
                     :enter #'hydra-file/body
                     :entry-hook 'evil-file-state-entry-hook
                     :exit-hook 'evil-file-state-exit-hook))


(provide 'rigpa-file-mode)
