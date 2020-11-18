(require 'chimera)
(require 'chimera-hydra)

(evil-define-state tab
  "Tab state."
  :tag " <T> "
  :message "-- TAB --"
  :enable (normal))

(defun setup-tab-marks-table ()
  "Initialize the tab marks hashtable and add an entry for the
current ('original') tab."
  (interactive)
  (defvar my-tab-marks-hash
    (make-hash-table :test 'equal))
  (save-tab 'original))

(defun save-tab (key)
  "Save current tab as original tab."
  (interactive)
  (puthash key (current-buffer)
           my-tab-marks-hash))

(defun load-tab (key)
  "Return to the buffer we were in at the time of entering
buffer mode."
  (interactive)
  (switch-to-buffer
   (gethash key my-tab-marks-hash)))

(defun flash-to-original-tab-and-back ()
  "Go momentarily to original tab and return.

This 'flash' allows the original tab, rather than the previous one
encountered while navigating to the present one, to be treated as the
last tab for 'flashback' ('Alt-tab') purposes. The flash should
happen quickly enough not to be noticeable."
  (interactive)
  (unless (equal (current-buffer) (my-original-tab))
    (let ((inhibit-redisplay t)) ;; not sure if this is doing anything but FWIW
      (load-tab 'original)
      (save-tab 'previous)
      (evil-switch-to-windows-last-buffer))))

(defun my-original-tab ()
  "Get original tab identifier"
  (interactive)
  (gethash 'original my-tab-marks-hash))

(defun return-to-original-tab ()
  "Return to the buffer we were in at the time of entering
buffer mode."
  (interactive)
  (load-tab 'original))

(defhydra hydra-tab (:color pink
                     :columns 2
                     :body-pre (setup-tab-marks-table) ; maybe put in ad-hoc entry function
                     :idle 1.0
                     :post (progn (flash-to-original-tab-and-back)
                                  (chimera-hydra-portend-exit "tab" t))
                     :after-exit (chimera-hydra-signal-exit "tab" #'chimera-handle-hydra-exit))
  "Tab mode"
  ("/" centaur-tabs-counsel-switch-group "search" :exit t)
  ("h" centaur-tabs-backward "previous")
  ("s-{" centaur-tabs-backward "previous") ; to "take over" from the global binding
  ("l" centaur-tabs-forward "next")
  ("s-}" centaur-tabs-forward "next") ; to "take over" from the global binding
  ("k" (lambda ()
         (interactive)
         (message "%s" (gethash 'original my-tab-marks-hash))
         (centaur-tabs-backward-group))
   "previous group")
  ("j" (lambda ()
         (interactive)
         (message "%s" (gethash 'original my-tab-marks-hash))
         (centaur-tabs-forward-group)) "next group")
  ("H" centaur-tabs-move-current-tab-to-left "move left")
  ("L" centaur-tabs-move-current-tab-to-right "move right")
  ("s-t" (lambda ()
           (interactive)
           (save-tab 'temp-previous)
           (load-tab 'previous)
           (puthash 'previous (gethash 'temp-previous my-tab-marks-hash)
                    my-tab-marks-hash)) "switch to last" :exit t)
  ("t" (lambda ()
         (interactive)
         (save-tab 'temp-previous)
         (load-tab 'previous)
         (puthash 'previous (gethash 'temp-previous my-tab-marks-hash)
                  my-tab-marks-hash)) "switch to last" :exit t)
  ("n" (lambda ()
         (interactive)
         (my-new-empty-buffer nil
                              :switch-p t)) "new" :exit t)
  ("x" kill-buffer "delete")
  ("?" my-buffer-info "info" :exit t)
  ("q" return-to-original-tab "return to original" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-tab-mode-entry-hook nil
  "Entry hook for epistemic tab mode.")

(defvar chimera-tab-mode-exit-hook nil
  "Exit hook for epistemic tab mode.")

(defvar chimera-tab-mode
  (make-chimera-mode :name "tab"
                     :enter #'hydra-tab/body
                     :entry-hook 'evil-tab-state-entry-hook
                     :exit-hook 'evil-tab-state-exit-hook))

;; register mode with the epistemic framework
(eem-register-mode chimera-tab-mode)


(provide 'eem-tab-mode)
