;; A "mode" to navigate buffers.
;; TODO: s-b to get into mode, ivy should already be visible, hjkl has
;; calling on, s to search-> exit hydra and go to
;; counsel-switch-buffer, s-b s-b to flashback, q to return to
;; original buffer, esc to exit mode

;; (defun my-ivy-switch-buffer ()
;;   (interactive)
;;   (keyboard-quit)
;;   (ivy-switch-buffer))

;; (define-prefix-command 'my-buffer-map)
;; (define-key my-buffer-map (kbd "j") 'ivy-next-line-and-call)
;; (define-key my-buffer-map (kbd "k") 'ivy-previous-line-and-call)
;; (define-key my-buffer-map (kbd "s") 'my-ivy-switch-buffer)
;; (define-key my-buffer-map (kbd "s-b") 'keyboard-quit)

;; (defun my-buffer-select ()
;;   (interactive)
;;   (setq ll (mapcar #'buffer-name (buffer-list)))
;;   (setq ll (append (cdr ll) (list (car ll))))
;;   (switch-to-buffer (car ll))
;;   (ivy-read "My buffers: " ll
;;             :action '(1 ;; index (1 based) of the default action
;;                       ("s" (lambda (x)
;;                              (switch-to-buffer x)) "switch"))
;;             :keymap my-buffer-map))

;; (global-set-key (kbd "s-b") 'my-buffer-select)

;; alternatively, try implementing as "advice" to ivy-switch-buffer
;; and provide a custom keymap. still, seems like it needs to be
;; managed inside a hydra

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

(defhydra hydra-buffer (:idle 1.0
                        :columns 3
                        :body-pre (progn (setup-buffer-marks-table)
                                         (evil-buffer-state))
                        :post (flash-to-original-and-back))
  "Buffer mode"
  ("s-b" evil-switch-to-windows-last-buffer "switch to last" :exit t)
  ("b" evil-switch-to-windows-last-buffer "switch to last" :exit t)
  ("h" previous-buffer "previous")
  ("l" next-buffer "next")
  ("n" (lambda ()
         (interactive)
         (my-new-empty-buffer nil
                              :switch-p t)) "new" :exit t)
  ("m" my-buffer-set-mark "set mark")
  ("'" my-buffer-return-to-mark "return to mark")
  ("`" my-buffer-return-to-mark "return to mark")
  ("s" my-search-buffers "search" :exit t)
  ("/" my-search-buffers "search" :exit t)
  ("i" ibuffer "list (ibuffer)" :exit t)
  ("x" kill-buffer "delete")
  ("?" my-buffer-info "info" :exit t)
  ("q" return-to-original-buffer "return to original" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

;; access the buffer menu via a "body" keybinding
(global-set-key (kbd "s-b") 'hydra-buffer/body)

(provide 'my-buffer-mode)
