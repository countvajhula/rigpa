(require 'chimera)
(require 'chimera-hydra)

(evil-define-state tab
  "Tab state."
  :tag " <T> "
  :message "-- TAB --"
  :enable (normal))

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

(defhydra hydra-tab (:columns 2
                     :body-pre (rigpa-tab-setup-marks-table) ; maybe put in ad-hoc entry function
                     :post (progn (rigpa-tab-flash-to-original)
                                  (chimera-hydra-portend-exit chimera-tab-mode t))
                     :after-exit (chimera-hydra-signal-exit chimera-tab-mode
                                                            #'chimera-handle-hydra-exit))
  "Tab mode"
  ("/" centaur-tabs-counsel-switch-group "search" :exit t)
  ("h" centaur-tabs-backward "previous")
  ("s-{" centaur-tabs-backward "previous") ; to "take over" from the global binding
  ("l" centaur-tabs-forward "next")
  ("s-}" centaur-tabs-forward "next") ; to "take over" from the global binding
  ("k" (lambda ()
         (interactive)
         (centaur-tabs-backward-group))
   "previous group")
  ("j" (lambda ()
         (interactive)
         (centaur-tabs-forward-group)) "next group")
  ("H" centaur-tabs-move-current-tab-to-left "move left")
  ("L" centaur-tabs-move-current-tab-to-right "move right")
  ("s-t" rigpa-tab-switch-to-last "switch to last" :exit t)
  ("t" rigpa-tab-switch-to-last "switch to last" :exit t)
  ("n" (lambda ()
         (interactive)
         (rigpa-buffer-create nil nil :switch-p t))
   "new" :exit t)
  ("x" kill-buffer "delete")
  ("?" rigpa-buffer-info "info" :exit t)
  ("q" rigpa-tab-return-to-original "return to original" :exit t)
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-tab-mode-entry-hook nil
  "Entry hook for rigpa tab mode.")

(defvar chimera-tab-mode-exit-hook nil
  "Exit hook for rigpa tab mode.")

(defvar chimera-tab-mode
  (make-chimera-mode :name "tab"
                     :enter #'hydra-tab/body
                     :pre-entry-hook 'chimera-tab-mode-entry-hook
                     :post-exit-hook 'chimera-tab-mode-exit-hook
                     :entry-hook 'evil-tab-state-entry-hook
                     :exit-hook 'evil-tab-state-exit-hook))


(provide 'rigpa-tab-mode)
