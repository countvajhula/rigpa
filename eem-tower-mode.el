(require 'eem-mode-mode)

;; Define evil states for each epistemic mode
(evil-define-state tower
  "Tower state."
  :tag " <塔> "
  :message "-- TOWER --"
  :enable (normal))

(setq eem--current-tower-index 0)
(setq eem--last-tower-index 0)
(setq eem--tower-index-on-entry 0)
(setq eem--flashback-tower-index 0)
(setq eem--current-level 1)  ;; TODO: set via hook in all modes incl evil modes
(setq eem--reference-buffer (current-buffer))
(make-variable-buffer-local 'eem--current-tower-index)
(make-variable-buffer-local 'eem--last-tower-index)
(make-variable-buffer-local 'eem--tower-index-on-entry)
(make-variable-buffer-local 'eem--flashback-tower-index)
(make-variable-buffer-local 'eem--current-level)

(defun eem--get-reference-buffer ()
  ; what's this for?
  "Get the buffer in reference to which epistemic mode is operating."
  (if (string-match (format "^%s"
                             eem-buffer-prefix)
                     (buffer-name))
      eem--reference-buffer
    (current-buffer)))

(defun eem--tower (tower-id)
  "The epistemic tower corresponding to the provided index."
  (interactive)
  (nth tower-id eem-towers))

(defun eem--current-tower ()
  "The epistemic editing tower we are currently in."
  (interactive)
  (with-current-buffer (eem--get-reference-buffer)
    (eem--tower eem--current-tower-index)))

(defun eem-previous-tower ()
  "Previous tower"
  (interactive)
  (with-current-buffer (eem--get-reference-buffer)
    (let ((tower-id (mod (- eem--current-tower-index
                           1)
                        (length eem-towers))))
     (eem--switch-to-tower tower-id))))

(defun eem-next-tower ()
  "Next tower"
  (interactive)
  (with-current-buffer (eem--get-reference-buffer)
    (let ((tower-id (mod (+ eem--current-tower-index
                           1)
                        (length eem-towers))))
     (eem--switch-to-tower tower-id))))

(defun eem--switch-to-tower (tower-id)
  "Switch to the tower indicated"
  (interactive)
  (let ((tower (eem--tower tower-id)))
    (switch-to-buffer (eem--buffer-name tower))
    (with-current-buffer (eem--get-reference-buffer)
      (setq eem--current-tower-index tower-id))
    (eem--extract-selected-level)))

(defun eem-render-tower (tower)
  "Render a text representation of an epistemic editing tower."
  (interactive)
  (let ((tower-buffer (my-new-empty-buffer
                       (eem--buffer-name tower)))
        (tower-levels (ht-get tower 'levels)))
    (let ((tower-height (length tower-levels)))
      (with-current-buffer tower-buffer
       (eem--set-buffer-appearance)
       (dolist
           (level-number (reverse
                          (number-sequence 0 (- tower-height
                                                1))))
         (let ((mode-name (nth level-number
                               tower-levels)))
           (insert "|―――"
                   (number-to-string level-number)
                   "―――|"
                   " " mode-name "\n")))
       (my-delete-line)))
    tower-buffer))

(defun my-enter-tower-mode ()
  "Enter a buffer containing a textual representation of the
initial epistemic tower."
  (interactive)
  (setq eem--reference-buffer (current-buffer))
  (dolist (tower eem-towers)
    (eem-render-tower tower))
  (with-current-buffer (eem--get-reference-buffer)
    ;; Store "previous" previous tower to support flashback
    ;; feature seamlessly. This is to get around hydra executing
    ;; functions after exiting rather than before, which loses
    ;; information about the previous tower if flashback is
    ;; being invoked. This is a hacky fix, but it works for now.
    ;; Improve this eventually.
    (setq eem--flashback-tower-index eem--tower-index-on-entry)
    (setq eem--tower-index-on-entry eem--current-tower-index)
    (eem--switch-to-tower eem--current-tower-index))
  (evil-tower-state))

(defun my-exit-tower-mode ()
  "Exit tower mode."
  (interactive)
  (with-current-buffer eem--reference-buffer
    (setq eem--last-tower-index eem--tower-index-on-entry))
  (eem--revert-buffer-appearance)
  (evil-normal-state) ; TODO: FIX
  (kill-matching-buffers (concat "^" eem-buffer-prefix) nil t))

(defun eem-flashback-to-last-tower ()
  "Switch to the last tower used.

Eventually this should be done via strange loop application
of buffer mode when in epistemic mode, or alternatively,
and perhaps equivalently, by treating 'switch tower' as the
monadic verb in the 'switch buffer' navigation."
  (interactive)
  (with-current-buffer (eem--get-reference-buffer)
    ;; setting hydra to exit here would be ideal, but it seems
    ;; the hydra exits prior to this function being run, and there's
    ;; no epistemic buffer to switch to. so for now, options are
    ;; either to manually hit enter to use the selected tower
    ;; or store the "previous" previous tower upon mode mode entry
    ;; to get around the need for that
    ;; (eem--switch-to-tower eem--last-tower-index)
    (let ((original-tower-index eem--current-tower-index))
      (setq eem--current-tower-index eem--flashback-tower-index)
      (setq eem--flashback-tower-index original-tower-index)
      ;; set the current level to the highest level in the new tower
      (setq eem--current-level (1- (length (ht-get (eem--current-tower)
                                                   'levels)))))))

(defhydra hydra-tower (:idle 1.0
                       :columns 4
                       :body-pre (my-enter-tower-mode)
                       :post (my-exit-tower-mode))
  "Tower mode"
  ;; Need a textual representation of the mode tower for these to operate on
  ("h" eem-previous-tower "previous tower")
  ("j" eem-select-next-level "lower level")
  ("k" eem-select-previous-level "higher level")
  ("l" eem-next-tower "next tower")
  ;; ("H" eem-highest-level "first level (recency)")
  ;; ("J" eem-lowest-level "lowest level")
  ;; ("K" eem-highest-level "highest level")
  ;; ("L" eem-lowest-level "last level (recency)")
  ;; different towers for different "major modes"
  ;; ("s-o" eem-mode-mru "Jump to most recent (like Alt-Tab)" :exit t)
  ;; ("o" eem-mode-mru :exit t)
  ;; with delete / change etc. we could construct towers and then select towers
  ;; there could be a maximal tower containing all the levels
  ;; ("/" eem-search "search")
  ;; move to change ordering of levels, an alternative to recency
  ;;
  ;; ffap other window -- open file with this other mode/tower: a formal "major mode"
  ;; the mode mode, tower mode, and so on recursively makes more sense
  ;; if we assume that keyboard shortcuts are scarce. this gives us ways to use
  ;; a small number of keys in any arbitrary configuration
  ("s-m" eem-flashback-to-last-tower "flashback" :exit t)  ; canonical action
  ("<return>" eem-enter-selected-level "enter selected level" :exit t)
  ("s-<return>" eem-enter-selected-level "enter selected level" :exit t)
  ("i" my-noop "exit" :exit t)
  ("<escape>" nil "exit" :exit t))
  ;("s-<return>" eem-enter-lower-level "enter lower level" :exit t)
  ;("s-<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(provide 'eem-tower-mode)
