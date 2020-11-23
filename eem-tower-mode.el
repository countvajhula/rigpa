(require 'eem-mode-mode)

(cl-defstruct editing-ensemble
  "Specification for an editing ensemble."
  name
  ;; TODO: members should be structs implementing an "entity" interface
  (members nil :documentation "A list of members of the editing ensemble.")
  (default nil :documentation "The canonical member of the tower."))

(defvar eem--current-tower-index 0)
(defvar eem--last-tower-index 0)
(defvar eem--tower-index-on-entry 0)
(defvar eem--flashback-tower-index 0)
(defvar eem--current-level 1)
;; the "ground" of buffers is a priori themselves,
;; representing a termination of the chain of reference
(defvar-local eem--ground-buffer nil)
(make-variable-buffer-local 'eem--current-tower-index)
(make-variable-buffer-local 'eem--last-tower-index)
(make-variable-buffer-local 'eem--tower-index-on-entry)
(make-variable-buffer-local 'eem--flashback-tower-index)
(make-variable-buffer-local 'eem--current-level)

(defun eem--get-ground-buffer ()
  "Get the ground buffer.

At the lowest level, the buffer is expected to refer to itself
to terminate the reference chain."
  (unless eem--ground-buffer
    (setq eem--ground-buffer (current-buffer)))
  eem--ground-buffer)

(cl-defgeneric eem-editing-entity-name (entity)
  "A generic function to access the name of any editing
entity, such as modes, towers or complexes.")

(cl-defmethod eem-editing-entity-name ((entity chimera-mode))
  (chimera-mode-name entity))

(cl-defmethod eem-editing-entity-name ((entity editing-ensemble))
  (editing-ensemble-name entity))

(defun eem-ensemble-member-position-by-name (ensemble name)
  (seq-position (seq-map #'eem-editing-entity-name
                         (editing-ensemble-members ensemble))
                name))

(defun eem-ensemble-size (ensemble)
  "Size of ensemble (e.g. height of a tower)."
  (length (editing-ensemble-members ensemble)))

(defun eem-ensemble-member-at-position (tower position)
  "Mode at LEVEL in the TOWER."
  (nth position (editing-ensemble-members tower)))

(defun eem--tower (tower-id)
  "The epistemic tower corresponding to the provided index."
  (interactive)
  (nth tower-id (editing-ensemble-members eem-general-complex)))

(defun eem--current-tower ()
  "The epistemic editing tower we are currently in."
  (interactive)
  (with-current-buffer (eem--get-ground-buffer)
    ;; TODO: sometimes at this point reference buffer
    ;; is *LV* (hydra menu display) instead of the
    ;; actual buffer
    (eem--tower eem--current-tower-index)))

(defun eem-previous-tower ()
  "Previous tower"
  (interactive)
  (with-current-buffer (eem--get-ground-buffer)
    (let ((tower-id (mod (- eem--current-tower-index
                           1)
                        (eem-ensemble-size eem-general-complex))))
     (eem--switch-to-tower tower-id))))

(defun eem-next-tower ()
  "Next tower"
  (interactive)
  (with-current-buffer (eem--get-ground-buffer)
    (let ((tower-id (mod (+ eem--current-tower-index
                           1)
                        (eem-ensemble-size eem-general-complex))))
     (eem--switch-to-tower tower-id))))

(defun eem--switch-to-tower (tower-id)
  "Switch to the tower indicated"
  (interactive)
  (let* ((tower (eem--tower tower-id))
         (tower-height (eem-ensemble-size tower))
         (current-mode-name (symbol-name evil-state))
         (level (or (eem-ensemble-member-position-by-name tower
                                                          current-mode-name)
                    (eem-ensemble-member-position-by-name tower
                                                          eem-recall)
                    0)))
    (switch-to-buffer (eem--buffer-name tower))
    (let ((start (progn (evil-goto-line 1) (line-beginning-position)))
          (end (progn (evil-goto-line tower-height) (line-end-position))))
      ;; only show the region that can be interacted with, don't show
      ;; the name of the tower
      (narrow-to-region start end))
    (evil-goto-line (- tower-height level))
    (with-current-buffer (eem--get-ground-buffer)
      (setq eem--current-tower-index tower-id))))

(defun eem--buffer-name (tower)
  "Buffer name to use for a given tower."
  (concat eem-buffer-prefix "-" (editing-ensemble-name tower)))

(defun eem-serialize-tower (tower)
  "A string representation of a tower."
  (let ((tower-height (eem-ensemble-size tower))
        (tower-str ""))
    (dolist
        (level-number (reverse
                       (number-sequence 0 (1- tower-height))))
      (let ((mode-name
             (eem-editing-entity-name
              (eem-ensemble-member-at-position tower
                                               level-number))))
        (setq tower-str
              (concat tower-str
                      "|―――"
                      (number-to-string level-number)
                      "―――|"
                      " " (if (equal mode-name (editing-ensemble-default tower))
                              (concat "[" mode-name "]")
                              mode-name)
                      "\n"))))
    (concat (string-trim tower-str)
            "\n"
            "\n-" (upcase (editing-ensemble-name tower)) "-")))

(defun eem-parse-tower (tower-str)
  "Derive a tower struct from a string representation."
  (make-editing-ensemble :name (parsec-with-input tower-str
                                 (eem--parse-tower-name))
                         :default (parsec-with-input tower-str
                                    (eem--parse-tower-default-mode))
                         :members (parsec-with-input tower-str
                                    (eem--parse-tower-level-names))))

(defun eem--set-meta-buffer-appearance ()
  "Configure meta mode buffer appearance."
  (buffer-face-set 'eem-face)
  (text-scale-set 5)
  ;;(setq cursor-type nil))
  (internal-show-cursor nil nil)
  (display-line-numbers-mode 'toggle)
  (hl-line-mode))

(defun eem-render-tower (tower)
  "Render a text representation of an epistemic editing tower in a buffer."
  (interactive)
  (let ((inherited-ground-buffer (eem--get-ground-buffer))
        (tower-buffer
         (my-new-empty-buffer (eem--buffer-name tower))))
    (with-current-buffer tower-buffer
      ;; ground buffer is inherited from the original
      ;; to define the chain of reference
      (setq eem--ground-buffer
            inherited-ground-buffer)
      (eem--set-meta-buffer-appearance)
      (insert (eem-serialize-tower tower)))
    tower-buffer))

(defun eem--set-ui-for-meta-modes ()
  "Set (for now, global) UI parameters for meta modes."
  ;; should ideally be perspective-specific
  (blink-cursor-mode -1))

(defun eem--revert-ui ()
  "Revert buffer appearance to settings prior to entering mode mode."
  (blink-cursor-mode 1))

(defun my-enter-tower-mode ()
  "Enter a buffer containing a textual representation of the
initial epistemic tower."
  (interactive)
  (dolist (tower (editing-ensemble-members eem-general-complex))
    (eem-render-tower tower))
  (with-current-buffer (eem--get-ground-buffer)
    ;; TODO: is it necessary to reference ground buffer here?
    ;;
    ;; Store "previous" previous tower to support flashback
    ;; feature seamlessly. This is to get around hydra executing
    ;; functions after exiting rather than before, which loses
    ;; information about the previous tower if flashback is
    ;; being invoked. This is a hacky fix, but it works for now.
    ;; Improve this eventually.
    (setq eem--flashback-tower-index eem--tower-index-on-entry)
    (setq eem--tower-index-on-entry eem--current-tower-index)
    (eem--switch-to-tower eem--current-tower-index))
  (eem--set-ui-for-meta-modes))

(defun my-exit-tower-mode ()
  "Exit tower mode."
  (interactive)
  (with-current-buffer (eem--get-ground-buffer)
    (setq eem--last-tower-index eem--tower-index-on-entry))
  (eem--revert-ui)
  (kill-matching-buffers (concat "^" eem-buffer-prefix) nil t)
  (switch-to-buffer (eem--get-ground-buffer)))

(defun eem-flashback-to-last-tower ()
  "Switch to the last tower used.

Eventually this should be done via strange loop application
of buffer mode when in epistemic mode, or alternatively,
and perhaps equivalently, by treating 'switch tower' as the
monadic verb in the 'switch buffer' navigation."
  (interactive)
  (with-current-buffer (eem--get-ground-buffer)
    ;; setting hydra to exit here would be ideal, but it seems
    ;; the hydra exits prior to this function being run, and there's
    ;; no epistemic buffer to switch to. so for now, options are
    ;; either to manually hit enter to use the selected tower
    ;; or store the "previous" previous tower upon mode mode entry
    ;; to get around the need for that
    ;; (eem--switch-to-tower eem--last-tower-index)
    (let ((original-tower-index eem--current-tower-index))
      (setq eem--current-tower-index eem--flashback-tower-index)
      (setq eem--flashback-tower-index original-tower-index))
    ;; enter the appropriate level in the new tower
    (eem--enter-appropriate-mode)))

(defun eem-enter-selected-level ()
  "Enter selected level"
  (interactive)
  (let ((selected-level (eem--extract-selected-level)))
    (with-current-buffer (eem--get-ground-buffer)
      (message "entering level %s in tower %s in buffer %s"
               selected-level
               eem--current-tower-index
               (current-buffer))
      (eem--enter-level selected-level))))


(provide 'eem-tower-mode)
