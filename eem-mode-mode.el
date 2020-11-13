(evil-define-state mode
  "Mode state."
  :tag " <M> "
  :message "-- MODE --"
  :enable (normal))

(defun eem-enter-mode (mode-name)
  "Enter mode MODE-NAME."
  (message "entering mode %s" mode-name)
  (let ((mode-entry (if (member mode-name (list "normal" "insert" "emacs"))
                        ;; handle the (at present, hypothetical) case of entry
                        ;; to a standard evil mode
                        ;; no hydra for standard evil modes
                        (intern (concat "evil-" mode-name "-state"))
                      (intern (concat "hydra-" mode-name "/body")))))
    (funcall mode-entry))
  (message "entered mode %s" mode-name))

(defun eem--enter-level (level-number)
  "Enter level LEVEL-NUMBER"
  (let* ((tower (eem--current-tower))
         (levels (ht-get tower 'levels))
         (tower-height (length levels))
         (level-number (max (min level-number
                                 (1- tower-height))
                            0)))
    (let ((mode-name (nth level-number levels)))
      (eem-enter-mode mode-name)
      (setq eem--current-level level-number))))

(defun eem-enter-lower-level ()
  "Enter lower level."
  (interactive)
  (message "entering lower level")
  (let ((mode-name (symbol-name evil-state)))
    (if (member mode-name (ht-get (eem--current-tower) 'levels))
        (progn (eem--update-mode-exit-flag mode-name)
               (eem--enter-level (- eem--current-level
                                    1)))
      ;; if we left a buffer in a state that isn't in its tower, then
      ;; returning to it "out of band" would find it still that way,
      ;; and Enter/Escape would assume that there is a recall to be
      ;; returned to. If we did nothing here, then since there is in
      ;; fact no recall, nothing would happen. So preemptively go
      ;; to a safe "normal" as a failsafe, which would be overridden
      ;; by a recall if there is one.
      ;; to reproduce: use C-] to jump to definition, to a buffer
      ;; that doesn't contain buffer mode in the tower, but which
      ;; was exited using buffer mode
      ;; TODO: Note that this is currently not safe since a tower not
      ;; containing normal mode (e.g. emacs tower) would be left in limbo
      (evil-normal-state)))) ; TODO: fix to sane normal

(defun eem-enter-higher-level ()
  "Enter higher level."
  (interactive)
  (message "entering higher level")
  (let ((mode-name (symbol-name evil-state)))
    ;; if the current mode is in the current tower,
    ;; go up a level and clear recall; otherwise do
    ;; nothing, and the mode exit hook would call
    ;; recall if one is set
    (if (member mode-name (ht-get (eem--current-tower) 'levels))
        (progn (eem--update-mode-exit-flag mode-name)
               (eem--enter-level (+ eem--current-level
                                    1)))
      ;; see note for eem-enter-lower-level
      (evil-normal-state)))) ; TODO: sane normal

(defun eem-enter-lowest-level ()
  "Enter lowest (manual) level."
  (interactive)
  (eem--enter-level 0))

(defun eem-enter-highest-level ()
  "Enter highest level."
  (interactive)
  (let* ((tower (eem--current-tower))
         (levels (ht-get tower 'levels))
         (tower-height (length levels)))
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

(defun eem-jump-to-level (mode)
  "Enter MODE, but remember the previous state to return to it."
  (interactive)
  ;; we're relying on the evil state here even though the
  ;; delegation is hydra -> evil. Probably introduce an
  ;; independent state variable, for which the evil state
  ;; variable can be treated as a proxy for now
  (message "entering %s with recall" mode)
  (setq-local eem-recall (symbol-name evil-state))
  (eem-enter-mode mode)
  ;; update current level if still in tower
  ;; TODO: not ideal to have this decoupled - streamline if possible
  (let ((level-number (seq-position (ht-get (eem--current-tower) 'levels) mode)))
    (when level-number
      (message "updating level number")
      (setq eem--current-level level-number))))

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
        (setq eem--current-level (1- (length (ht-get (eem--current-tower)
                                                     'levels))))
        (eem--enter-level eem--current-level)))))

(defun eem--enter-local-recall-mode (&optional buffer)
  "Enter the recall mode (if any) in the BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((recall (and (boundp 'eem-recall)
                       eem-recall)))
      (if recall
          ;; recall should probably be tower-specific and
          ;; meta-level specific, so that
          ;; we can set it upon entry to a meta mode
          (progn (setq-local eem-recall nil)
                 (eem-enter-mode recall))
        ;; for now, enter the highest level in the absence of recall
        (setq eem--current-level (1- (length (ht-get (eem--current-tower)
                                                     'levels))))
        (eem--enter-level eem--current-level)))))

;; move these to epistemic.el?
(defun eem--update-mode-exit-flag (mode &optional value)
  "Set a mode exit flag to indicate cleanup operations need to be performed."
  (message "updating flag: %s %s" mode value)
  (let ((hydra (intern (concat "hydra-" mode))))
    (hydra-set-property hydra :exiting value)))

(defun eem-handle-mode-exit (mode)
  "Take appropriate action when MODE is exited.

Recalls a prior state upon exiting MODE, if one is indicated."
  (message "exiting %s with recall" mode)
  (eem--enter-local-recall-mode))

(defun eem-hydra-signal-exit (mode)
  "Helper function to witness hydra exit and notify epistemic mode."
  (let ((hydra (intern (concat "hydra-" mode))))
    (when (hydra-get-property hydra :exiting)
      (eem-handle-mode-exit mode)
      (hydra-set-property hydra :exiting nil))))


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
