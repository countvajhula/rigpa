(require 'rigpa-mode-mode)
(require 'rigpa-types)

;; make these buffer-local first before abstracting "state"
(defvar rigpa--current-tower-index 0)
(defvar rigpa--last-tower-index 0)
(defvar rigpa--tower-index-on-entry 0)
(defvar rigpa--flashback-tower-index 0)
(defvar rigpa--current-level 0)
;; the "ground" of buffers is a priori themselves,
;; representing a termination of the chain of reference
(defvar-local rigpa--ground-buffer nil)
(make-variable-buffer-local 'rigpa--current-tower-index)
(make-variable-buffer-local 'rigpa--last-tower-index)
(make-variable-buffer-local 'rigpa--tower-index-on-entry)
(make-variable-buffer-local 'rigpa--flashback-tower-index)
(make-variable-buffer-local 'rigpa--current-level)

(defun rigpa--tower (tower-id)
  "The editing tower corresponding to the provided index."
  (interactive)
  (nth tower-id (editing-ensemble-members rigpa--complex)))

(defun rigpa--ground-tower ()
  "The editing tower we are currently in, in relation to the ground buffer."
  (interactive)
  (with-current-buffer (rigpa--get-ground-buffer)
    ;; TODO: sometimes at this point reference buffer
    ;; is *LV* (hydra menu display) instead of the
    ;; actual buffer
    (rigpa--tower rigpa--current-tower-index)))

(defun rigpa--local-tower (&optional buffer)
  "The editing tower we are in, in relation to the present buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (rigpa--tower rigpa--current-tower-index)))

(defun rigpa--tower-view-reflect-ground (tower)
  "Assuming a representation of TOWER in the present buffer, reflect ground state in it."
  (let* ((ground-buffer (rigpa--get-ground-buffer))
         (ground-mode-name (with-current-buffer ground-buffer
                             (symbol-name evil-state)))
         (ground-recall (with-current-buffer ground-buffer
                          rigpa-recall))
         (level (or (rigpa-ensemble-member-position-by-name tower
                                                          ground-mode-name)
                    (and ground-recall
                         (rigpa-ensemble-member-position-by-name tower
                                                               ground-recall))
                    0))
         (tower-height (rigpa-ensemble-size tower)))
    (evil-goto-line (- tower-height level))))

(defun rigpa--tower-view-narrow (tower)
  "Narrow view to actionable part, and reflect ground state."
  (let* ((tower-height (rigpa-ensemble-size tower))
         (start (save-excursion (goto-char (point-min))
                                (point)))
         (end (save-excursion (goto-char (point-min))
                              (forward-line (1- tower-height))
                              (line-end-position))))
    ;; only show the region that can be interacted with, don't show
    ;; the name of the tower
    (narrow-to-region start end)))

(defun rigpa--switch-to-tower (tower-id)
  "View the tower indicated, reflecting the state of the ground buffer."
  ;; "view" tower
  ;; this should be replaced with meta buffer mode and any applicable side-effects
  (interactive)
  (let ((tower (rigpa--tower tower-id)))
    (switch-to-buffer (rigpa--buffer-name tower))
    (rigpa--tower-view-narrow tower)
    (rigpa--tower-view-reflect-ground tower)
    (with-current-buffer (rigpa--get-ground-buffer)
      ;; ad hoc modeling of buffer mode side effect here
      (setq rigpa--current-tower-index tower-id))))

(defun rigpa-serialize-tower (tower)
  "A string representation of a tower."
  (let ((tower-height (rigpa-ensemble-size tower))
        (tower-str ""))
    (dolist
        (level-number (reverse
                       (number-sequence 0 (1- tower-height))))
      (let ((mode (rigpa-ensemble-member-at-position tower
                                                   level-number)))
        (setq tower-str
              (concat tower-str
                      (rigpa-serialize-mode mode)
                      "\n"))))
    (concat (string-trim tower-str)
            "\n"
            "\n" ":" (upcase (editing-ensemble-name tower)) ":")))

(defun rigpa-parse-tower (tower-str)
  "Derive a tower struct from a string representation."
  (let* ((level-names (rigpa--parse-level-names tower-str))
         (levels (seq-map (lambda (name)
                            (ht-get rigpa-modes name))
                          level-names)))
    (make-editing-ensemble :name (rigpa--parse-tower-name tower-str)
                           :default (rigpa--parse-tower-default-name tower-str)
                           :members levels)))

(defun rigpa-parse-tower-from-buffer (&optional buffer)
  "Parse a tower struct from a BUFFER containing a text representation of it."
  (with-current-buffer (or buffer (current-buffer))
    (widen)
    (message "PARSING: %s" (buffer-string))
    (let ((tower (rigpa-parse-tower (buffer-string))))
      (rigpa--tower-view-narrow tower)
      tower)))

(defun rigpa-render-tower-for-mode-mode (tower &optional major-mode)
  "Render a text representation of an editing tower in a buffer."
  (interactive)
  (let* ((ground (current-buffer))
         (major-mode (or major-mode #'rigpa-meta-mode))
         (buffer (rigpa-buffer-create (rigpa--buffer-name tower)
                                      major-mode)))
    (with-current-buffer buffer
      ;; ground buffer is inherited from the original
      ;; to define the chain of reference
      (setq rigpa--ground-buffer
            ground)
      (rigpa--set-meta-buffer-appearance)
      (insert (rigpa-serialize-tower tower))
      (rigpa--enter-appropriate-mode))
    buffer))

(defun rigpa-render-tower (tower &optional major-mode)
  "Render a text representation of an editing tower in a buffer."
  (interactive)
  (let* ((inherited-ground-buffer (rigpa--get-ground-buffer))
         (major-mode (or major-mode #'rigpa-meta-mode))
         (buffer (rigpa-buffer-create (rigpa--buffer-name tower)
                                      major-mode)))
    (with-current-buffer buffer
      ;; ground buffer is inherited from the original
      ;; to define the chain of reference
      (setq rigpa--ground-buffer
            inherited-ground-buffer)
      (rigpa--set-meta-buffer-appearance)
      (insert (rigpa-serialize-tower tower))
      (rigpa--enter-appropriate-mode))
    buffer))

(defun rigpa-flashback-to-last-tower ()
  "Switch to the last tower used.

Eventually this should be done via strange loop application
of buffer mode when in a rigpa meta mode, or alternatively,
and perhaps equivalently, by treating 'switch tower' as the
monadic verb in the 'switch buffer' navigation."
  (interactive)
  (with-current-buffer (rigpa--get-ground-buffer)
    ;; setting hydra to exit here would be ideal, but it seems
    ;; the hydra exits prior to this function being run, and there's
    ;; no meta buffer to switch to. so for now, options are
    ;; either to manually hit enter to use the selected tower
    ;; or store the "previous" previous tower upon mode mode entry
    ;; to get around the need for that
    ;; (rigpa--switch-to-tower rigpa--last-tower-index)
    (let ((original-tower-index rigpa--current-tower-index))
      (setq rigpa--current-tower-index rigpa--flashback-tower-index)
      (setq rigpa--flashback-tower-index original-tower-index))
    ;; enter the appropriate level in the new tower
    (rigpa--enter-appropriate-mode)))

(defun rigpa-previous-tower ()
  "Previous tower"
  (interactive)
  (with-current-buffer (rigpa--get-ground-buffer)
    (let ((tower-id (mod (1- rigpa--current-tower-index)
                         (rigpa-ensemble-size rigpa--complex))))
     (rigpa--switch-to-tower tower-id))))

(defun rigpa-next-tower ()
  "Next tower"
  (interactive)
  (with-current-buffer (rigpa--get-ground-buffer)
    (let ((tower-id (mod (1+ rigpa--current-tower-index)
                         (rigpa-ensemble-size rigpa--complex))))
     (rigpa--switch-to-tower tower-id))))

;; probably what we want is:
;; 1. set up a "primary" buffer ring for all buffers at init time
;;    and ensure all open buffers in buffer-list are part of it
;; 2. use buffer-ring-next/previous in buffer mode
;; 3. for tower mode,
;;    (1) switch to a new "tower" ring, and
;;    (2) add a side effect to buffer-ring-next/previous
;;        to modify the tower index in the ground buffer
;; Later, worry about moving all of these to "coordinates"
;; and also about improving the buffer ring interface with
;; explicit constructors and so on, and performance with
;; hashes instead of simple conses if it becomes a problem
(defun rigpa--previous-tower-wrapper (orig-fn &rest args)
  "Thin wrapper to disregard actual buffer change functions (temporary hack)."
  (rigpa-previous-tower))

(defun rigpa--next-tower-wrapper (orig-fn &rest args)
  "Thin wrapper to disregard actual buffer change functions (temporary hack)."
  (rigpa-next-tower))

(defun rigpa--add-meta-tower-side-effects ()
  "Add side effects for primitive mode operations while in meta mode."
  ;; this should lookup the appropriate side-effect based on the coordinates
  (advice-add #'previous-buffer :around #'rigpa--previous-tower-wrapper)
  (advice-add #'next-buffer :around #'rigpa--next-tower-wrapper))

;; ensure the meta and meta-tower's are straight
(defun rigpa--remove-meta-tower-side-effects ()
  "Remove side effects for primitive mode operations that were added for meta modes."
  (advice-remove #'previous-buffer #'rigpa--previous-tower-wrapper)
  (advice-remove #'next-buffer #'rigpa--next-tower-wrapper))

(defun rigpa-enter-tower-mode ()
  "Enter a buffer containing a textual representation of the
initial editing tower."
  (interactive)
  (with-current-buffer (rigpa--get-ground-buffer)
    (dolist (tower (editing-ensemble-members rigpa--complex))
      (rigpa-render-tower tower #'rigpa-meta-tower-mode)))
  ;; TODO: is it necessary to reference ground buffer here?
  ;;
  ;; Store "previous" previous tower to support flashback
  ;; feature seamlessly. This is to get around hydra executing
  ;; functions after exiting rather than before, which loses
  ;; information about the previous tower if flashback is
  ;; being invoked. This is a hacky fix, but it works for now.
  ;; Improve this eventually.
  (setq rigpa--flashback-tower-index rigpa--tower-index-on-entry)
  (setq rigpa--tower-index-on-entry rigpa--current-tower-index)
  (with-current-buffer (rigpa--get-ground-buffer)
    (rigpa--switch-to-tower rigpa--current-tower-index))
  (rigpa--add-meta-tower-side-effects)
  (rigpa--set-ui-for-meta-modes))

(defun rigpa-exit-tower-mode ()
  "Exit tower mode."
  (interactive)
  (let ((ref-buf (rigpa--get-ground-buffer)))
    (with-current-buffer ref-buf
      (setq rigpa--last-tower-index rigpa--tower-index-on-entry))
    (rigpa--revert-ui)
    (rigpa--remove-meta-tower-side-effects)
    (kill-matching-buffers (concat "^" rigpa-buffer-prefix) nil t)
    (switch-to-buffer ref-buf)))

(defhydra hydra-tower (:idle 1.0
                       :columns 4
                       :body-pre (rigpa-enter-tower-mode)
                       :post (rigpa-exit-tower-mode))
  "Tower mode"
  ;; Need a textual representation of the mode tower for these to operate on
  ("h" rigpa-previous-tower "previous tower")
  ("j" rigpa-select-next-level "lower level")
  ("k" rigpa-select-previous-level "higher level")
  ("l" rigpa-next-tower "next tower")
  ;; ("H" rigpa-highest-level "first level (recency)")
  ;; ("J" rigpa-lowest-level "lowest level")
  ;; ("K" rigpa-highest-level "highest level")
  ;; ("L" rigpa-lowest-level "last level (recency)")
  ;; different towers for different "major modes"
  ;; ("s-o" rigpa-mode-mru "Jump to most recent (like Alt-Tab)" :exit t)
  ;; ("o" rigpa-mode-mru :exit t)
  ;; with delete / change etc. we could construct towers and then select towers
  ;; there could be a maximal tower containing all the levels
  ;; ("/" rigpa-search "search")
  ;; move to change ordering of levels, an alternative to recency
  ;;
  ;; ffap other window -- open file with this other mode/tower: a formal "major mode"
  ;; the mode mode, tower mode, and so on recursively makes more sense
  ;; if we assume that keyboard shortcuts are scarce. this gives us ways to use
  ;; a small number of keys in any arbitrary configuration
  ("s-m" rigpa-flashback-to-last-tower "flashback" :exit t)  ; canonical action
  ("<return>" rigpa-enter-selected-level "enter selected level" :exit t)
  ("s-<return>" rigpa-enter-selected-level "enter selected level" :exit t)
  ("i" nil "exit" :exit t)
  ("<escape>" nil "exit" :exit t))

  ;("s-<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ;("s-<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(provide 'rigpa-tower-mode)
