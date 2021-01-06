(require 'chimera)
(require 'chimera-hydra)

(evil-define-state activity
  "Activity state."
  :tag " <A> "
  :message "-- ACTIVITY --"
  :enable (normal))

(setq rigpa-activity-accumulate-buffer-name "MY-CLIPBOARD")

(defun rigpa-activity-yank-and-accumulate ()
  "'Yank'/accumulate text in a temporary buffer."
  (interactive)
  (unless (get-buffer rigpa-activity-accumulate-buffer-name)
    (rigpa-buffer-create rigpa-activity-accumulate-buffer-name))
  (with-current-buffer rigpa-activity-accumulate-buffer-name
    (goto-char (point-max)))
  (append-to-buffer rigpa-activity-accumulate-buffer-name
                    (if (region-active-p)
                        (region-beginning)
                      (line-beginning-position))
                    (if (region-active-p)
                        (region-end)
                      (line-end-position)))
  (with-current-buffer rigpa-activity-accumulate-buffer-name
    (goto-char (point-max))
    (insert "\n"))
  (deactivate-mark))

(defun rigpa-activity-paste-and-clear ()
  "Paste contents of paste buffer and empty it."
  (interactive)
  (insert-buffer rigpa-activity-accumulate-buffer-name)
  (kill-buffer rigpa-activity-accumulate-buffer-name))

(defun rigpa-activity-previous ()
  "docstring"
  (interactive)
  (call-interactively 'goto-last-change))

(defun rigpa-activity-next ()
  "docstring"
  (interactive)
  (when (eq last-command 'rigpa-activity-previous)
    ;; emacs only cycles "forwards" through the change list
    ;; if the previous command was a "backwards" navigation
    ;; through this list. Since we're wrapping the internal
    ;; commands, we need to manually indicate that the last
    ;; command was equivalent to the internal backwards
    ;; changelist navigation command
    (setq last-command 'goto-last-change))
  (call-interactively 'goto-last-change-reverse))


(defhydra hydra-activity (:columns 2
                          :post (chimera-hydra-portend-exit chimera-activity-mode t)
                          :after-exit (chimera-hydra-signal-exit chimera-activity-mode
                                                                 #'chimera-handle-hydra-exit))
  "Activity mode"
  ("h" rigpa-activity-previous "previous change in buffer")
  ("C-j" evil-jump-backward "jump backward") ;; TODO: these jumps don't work via hydra atm
  ("C-k" evil-jump-forward "jump forward")
  ("l" rigpa-activity-next "next change in buffer")
  ("m" evil-set-marker "set mark")
  ("g" evil-goto-mark "go to mark")
  ("y" rigpa-activity-yank-and-accumulate "yank and accumulate") ;; TODO: these don't work via hydra atm
  ("p" rigpa-activity-paste-and-clear "paste and clear")
  ("a" rigpa-activity-previous "previous change in buffer" :exit t)
  ("s-a" rigpa-activity-previous "previous change in buffer" :exit t)
  ("i" ignore "exit" :exit t)
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-activity-mode-entry-hook nil
  "Entry hook for rigpa activity mode.")

(defvar chimera-activity-mode-exit-hook nil
  "Exit hook for rigpa activity mode.")

(defvar chimera-activity-mode
  (make-chimera-mode :name "activity"
                     :enter #'hydra-activity/body
                     :entry-hook 'evil-activity-state-entry-hook
                     :exit-hook 'evil-activity-state-exit-hook))


(provide 'rigpa-activity-mode)
