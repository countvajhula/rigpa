(evil-define-state mode
  "Mode state."
  :tag " <M> "
  :message "-- MODE --"
  :enable (normal))

(defun eem-enter-mode (mode-name)
  "Enter mode MODE-NAME."
  (chimera-enter-mode mode-name))

(defun eem--enter-level (level-number)
  "Enter level LEVEL-NUMBER"
  (let* ((tower (eem--current-tower))
         (tower-height (eem-tower-height tower))
         (level-number (max (min level-number
                                 (1- tower-height))
                            0)))
    (let ((mode-name (eem-tower-mode-at-level tower level-number)))
      (eem-enter-mode mode-name)
      (setq eem--current-level level-number))))

(defun eem-enter-lower-level ()
  "Enter lower level."
  (interactive)
  (message "entering lower level")
  (let ((mode-name (symbol-name evil-state)))
    (if (eem-tower-level-of-mode (eem--current-tower)
                                 mode-name)
        (when (> eem--current-level 0)
          (eem--enter-level (1- eem--current-level)))
      ;; if we left a buffer in a state that isn't in its tower, then
      ;; returning to it "out of band" would find it still that way,
      ;; and Enter/Escape would do nothing since the mode is still
      ;; outside the local tower. Ordinarily, we would return to this
      ;; buffer in an epistemic mode such as buffer mode, which upon
      ;; exiting would look for a recall. Since that isn't the case
      ;; here, nothing would happen at this point. So preemptively go
      ;; to a safe "default" as a failsafe, which would be overridden
      ;; by a recall if there is one.
      (let ((default-mode (eem-tower-default-mode (eem--current-tower))))
        (eem-enter-mode default-mode)
        (message "Not in tower, couldn't take the stairs; entered tower default: %s. Level is %s."
                 default-mode
                 eem--current-level)))))

(defun eem-enter-higher-level ()
  "Enter higher level."
  (interactive)
  (message "entering higher level")
  (let ((mode-name (symbol-name evil-state)))
    ;; if the current mode is in the current tower,
    ;; go up a level and clear recall; otherwise do
    ;; nothing, and the mode exit hook would call
    ;; recall if one is set
    (if (eem-tower-level-of-mode (eem--current-tower)
                                 mode-name)
        (when (< eem--current-level
                 (1- (eem-tower-height (eem--current-tower))))
          (eem--enter-level (1+ eem--current-level)))
      ;; see note for eem-enter-lower-level
      (let ((default-mode (eem-tower-default-mode (eem--current-tower))))
        (eem-enter-mode default-mode)
        (message "Not in tower, couldn't take the stairs; entered tower default: %s. Level is %s."
                 default-mode
                 eem--current-level)))))

(defun eem-enter-lowest-level ()
  "Enter lowest (manual) level."
  (interactive)
  (eem--enter-level 0))

(defun eem-enter-highest-level ()
  "Enter highest level."
  (interactive)
  (let* ((tower (eem--current-tower))
         (tower-height (eem-tower-height tower)))
    (eem--enter-level (- tower-height
                         1))))

(defun eem--extract-selected-level ()
  "Extract the selected level from the current representation"
  (interactive)
  (let* ((level-str (thing-at-point 'line t))
         (level-number (string-to-number (progn (string-match "[[:digit:]]+"
                                                              level-str)
                                                (match-string 0 level-str)))))
    (setq eem--selected-level level-number)))

(defun eem-select-previous-level ()
  "Select previous level"
  (interactive)
  (evil-previous-line)
  (eem--extract-selected-level))

(defun eem-select-next-level ()
  "Select next level"
  (interactive)
  (evil-next-line)
  (eem--extract-selected-level))

(defun eem-reconcile-level ()
  "Adjust level to match current mode.

If the current mode is present in the current tower, ensure that the
current level reflects the mode's position in the tower."
  (interactive)
  ;; TODO: not ideal to have this decoupled - streamline if possible
  (let* ((mode-name (symbol-name evil-state))
         (level-number
          (eem-tower-level-of-mode (eem--current-tower)
                                   mode-name)))
    (when level-number
      (setq eem--current-level level-number)
      (message "mode %s is in tower; updated level number to %s and clearing recall %s"
               mode-name
               eem--current-level
               (and (boundp 'eem-recall) eem-recall))
      ;; clear recall since we know we're still in the tower
      (eem--clear-local-recall))))

(defun eem--enter-appropriate-mode (&optional buffer)
  "Enter the most appropriate mode. TODO: not used at the moment.

Priority: (1) provided mode if admissible (i.e. present in tower)
(2) recall if present, (3) lowest level (or maybe 'tower home level' which defaults to lowest)."
  (with-current-buffer (or buffer (current-buffer))
    (let ((recall (and (boundp 'eem-recall)
                       eem-recall)))
      (if recall
          (eem--enter-local-recall-mode)
        ;; for now, enter the highest level in the absence of recall
        (eem-enter-highest-level)))))

(defun eem--clear-local-recall (&optional buffer)
  "Clear recall flag if any."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local eem-recall nil)
    (message "cleared recall!")))

(defun eem--enter-local-recall-mode (&optional buffer)
  "Enter the recall mode (if any) in the BUFFER.

This should generally not be called directly but rather via
hooks. Only call it directly when entering a recall mode
is precisely the thing to be done."
  (with-current-buffer (or buffer (current-buffer))
    (let ((recall (and (boundp 'eem-recall)
                       eem-recall)))
      (eem--clear-local-recall)
      (eem-enter-mode recall))))

(defun eem-remember-or-recall (&optional buffer)
  "Remember the current mode for future recall, or recall to an earlier mode."
  ;; we're relying on the evil state here even though the
  ;; delegation is hydra -> evil. Probably introduce an
  ;; independent state variable, for which the evil state
  ;; variable can be treated as a proxy for now
  (with-current-buffer (or buffer (current-buffer))
    (let ((mode-name (symbol-name evil-state))
          (recall (and (boundp 'eem-recall)
                       eem-recall)))
      ;; recall should probably be tower-specific and
      ;; meta-level specific, so that
      ;; we can set it upon entry to a meta mode
      (when recall
        ;; if recall were determined to be irrelevant, the flag
        ;; would have been cleared by this point. If it's still here,
        ;; then this is a transient state and we need to recall.
        (progn (message "exiting %s, recall present: %s" mode-name recall)
               (eem--clear-local-recall)
               (eem-enter-mode recall)
               (message "exited %s, recalled %s" mode-name recall))
        ;; otherwise, let's remember the current state for
        ;; possible recall
        ;; TODO: this is getting called after mode has already changed to
        ;; the new one (e.g. via escape-higher), so this saves the new
        ;; mode as recall instead of the exiting mode
        ))))

(defun eem-set-mode-recall (mode-name)
  "Remember the current state to 'recall' it later."
  (setq-local eem-recall mode-name))


(defhydra hydra-mode (:idle 1.0
                      :columns 4
                      :body-pre (evil-mode-state))
  "Mode mode"
  ("j" eem-mode-down "down")
  ("k" eem-mode-up "up")
  ;; TODO: superlatives for selection
  ("J" eem-move-down "move down")
  ("K" eem-move-up "move up")
  ;; TODO: superlatives for moving
  ("+" eem-make-mode "make")  ; allow ivy selection from all registered modes
  ("x" eem-delete-mode "delete")
  ("c" eem-change-mode "change")
  ("m" eem-other-mode "Return to most recent (like Alt-Tab)" :exit t)
  ("u" eem-mode-undo "undo")
  ("C-r" eem-mode-redo "redo")
  ("/" eem-mode-search "search") ; ivy search full list of modes (alternative to s-<> entry)
  ("=" eem-restore-mode "restore") ; to factory defaults?
  ("i" nil "exit" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t) ;; ?
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t)) ;; ?

(global-set-key (kbd "s-k") (lambda ()
                              (interactive)
                              (eem-jump-to-level "mode")))

;; mode mode as the lowest level upon s-Esc, with tower mode above that achieved via s-Esc again, and so on...
;; i.e. once in any meta mode, you should be able to use the usual L00 machinery incl. e.g. line mode
;; maybe tower mode should only operate on towers - and mode mode could take advantage of a similar (but more minimal) representation as tower mode currently has

(provide 'eem-mode-mode)
