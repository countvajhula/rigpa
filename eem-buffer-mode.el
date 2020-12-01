(require 'chimera)
(require 'chimera-hydra)

(evil-define-state buffer
  "Buffer state."
  :tag " <B> "
  :message "-- BUFFER --"
  :enable (normal))

(defun my-buffer-set-mark (mark-name)
  "Set a mark"
  (interactive "cMark name?")
  (puthash mark-name (current-buffer) my-buffer-marks-hash)
  (message "Mark '%c' set." mark-name))

(defun my-buffer-return-to-mark (mark-name)
  "Return to mark"
  (interactive "cMark name?")
  (switch-to-buffer (gethash mark-name my-buffer-marks-hash)))

(defun return-to-original-buffer ()
  "Return to the buffer we were in at the time of entering
buffer mode."
  (interactive)
  (switch-to-buffer (my-original-buffer)))

(defun flash-to-original-and-back ()
  "Go momentarily to original buffer and return.

This 'flash' allows the original buffer, rather than the previous one
encountered while navigating to the present one, to be treated as the
last buffer for 'flashback' ('Alt-tab') purposes. The flash should
happen quickly enough not to be noticeable."
  (interactive)
  (unless (equal (current-buffer) (my-original-buffer))
    (let ((inhibit-redisplay t)) ;; not sure if this is doing anything but FWIW
      (return-to-original-buffer)
      (evil-switch-to-windows-last-buffer))))

(defun setup-buffer-marks-table ()
  "Initialize the buffer marks hashtable and add an entry for the
current ('original') buffer."
  (interactive)
  (defvar my-buffer-marks-hash
    (make-hash-table :test 'equal))
  (save-original-buffer))

(defun save-original-buffer ()
  "Save current buffer as original buffer."
  (interactive)
  (puthash "0" (current-buffer)
           my-buffer-marks-hash))

(defun my-original-buffer ()
  "Get original buffer identifier"
  (interactive)
  (gethash "0" my-buffer-marks-hash))

(defun my-search-buffers ()
  "Search for buffer."
  (interactive)
  (return-to-original-buffer)
  (ivy-switch-buffer))

;; TODO: implement a dynamic ring buffer storing every visited buffer
;; then, buffer mode retains a pointer to the current position (buffer)
;; and reverses direction of traversal each time buffer mode is exited
;; h and l should then use this wrapped form of previous and next buffer
;; we also wouldn't need to rely on evil-switch-to-windows-last-buffer
;; so there would be no need to "flash back"
;; this should be an independent utility library so that it can be used
;; in all modes that need an intuitive way to keep track of recency
;; maybe `recency-ring`, has a nice... ring to it
(defhydra hydra-buffer (:columns 3
                        :body-pre (setup-buffer-marks-table) ; maybe put in ad-hoc entry
                        :post (progn (flash-to-original-and-back)
                                     (chimera-hydra-portend-exit chimera-buffer-mode t))
                        :after-exit (chimera-hydra-signal-exit chimera-buffer-mode
                                                               #'chimera-handle-hydra-exit))
  "Buffer mode"
  ("s-b" evil-switch-to-windows-last-buffer "switch to last" :exit t)
  ("b" evil-switch-to-windows-last-buffer "switch to last" :exit t)
  ("h" previous-buffer "previous")
  ("j" ignore nil)
  ("k" ignore nil)
  ("l" next-buffer "next")
  ("n" (lambda ()
         (interactive)
         (my-new-empty-buffer nil nil :switch-p t))
   "new" :exit t)
  ("m" my-buffer-set-mark "set mark")
  ("'" my-buffer-return-to-mark "return to mark" :exit t)
  ("`" my-buffer-return-to-mark "return to mark" :exit t)
  ("s" my-search-buffers "search" :exit t)
  ("/" my-search-buffers "search" :exit t)
  ("i" ibuffer "list (ibuffer)" :exit t)
  ("x" kill-buffer "delete")
  ("?" my-buffer-info "info" :exit t)
  ("q" return-to-original-buffer "return to original" :exit t)
  ("H-m" eem-toggle-menu "show/hide this menu")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-buffer-mode-entry-hook nil
  "Entry hook for epistemic buffer mode.")

(defvar chimera-buffer-mode-exit-hook nil
  "Exit hook for epistemic buffer mode.")

(defvar chimera-buffer-mode
  (make-chimera-mode :name "buffer"
                     :enter #'hydra-buffer/body
                     :entry-hook 'evil-buffer-state-entry-hook
                     :exit-hook 'evil-buffer-state-exit-hook))


(provide 'eem-buffer-mode)
