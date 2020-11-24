(require 'eem-mode-mode)
(require 'eem-types)

;; make these buffer-local first before abstracting "state"
(defvar eem--current-tower-index 0)
(defvar eem--last-tower-index 0)
(defvar eem--tower-index-on-entry 0)
(defvar eem--flashback-tower-index 0)
(defvar eem--current-level 0)
;; the "ground" of buffers is a priori themselves,
;; representing a termination of the chain of reference
(defvar-local eem--ground-buffer nil)
(make-variable-buffer-local 'eem--current-tower-index)
(make-variable-buffer-local 'eem--last-tower-index)
(make-variable-buffer-local 'eem--tower-index-on-entry)
(make-variable-buffer-local 'eem--flashback-tower-index)
(make-variable-buffer-local 'eem--current-level)

(defun eem--tower (tower-id)
  "The epistemic tower corresponding to the provided index."
  (interactive)
  (nth tower-id (editing-ensemble-members eem--complex)))

(defun eem--ground-tower ()
  "The editing tower we are currently in, in relation to the ground buffer."
  (interactive)
  (with-current-buffer (eem--get-ground-buffer)
    ;; TODO: sometimes at this point reference buffer
    ;; is *LV* (hydra menu display) instead of the
    ;; actual buffer
    (eem--tower eem--current-tower-index)))

(defun eem--local-tower (&optional buffer)
  "The editing tower we are in, in relation to the present buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (eem--tower eem--current-tower-index)))

(defun eem-previous-tower ()
  "Previous tower"
  (interactive)
  (with-current-buffer (eem--get-ground-buffer)
    (let ((tower-id (mod (1- eem--current-tower-index)
                         (eem-ensemble-size eem--complex))))
     (eem--switch-to-tower tower-id))))

(defun eem-next-tower ()
  "Next tower"
  (interactive)
  (with-current-buffer (eem--get-ground-buffer)
    (let ((tower-id (mod (1+ eem--current-tower-index)
                         (eem-ensemble-size eem--complex))))
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

(defun eem-serialize-tower (tower)
  "A string representation of a tower."
  (let ((tower-height (eem-ensemble-size tower))
        (tower-str ""))
    (dolist
        (level-number (reverse
                       (number-sequence 0 (1- tower-height))))
      (let ((mode (eem-ensemble-member-at-position tower
                                                   level-number)))
        (setq tower-str
              (concat tower-str
                      (eem-serialize-mode mode)
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

(defun eem-render-tower (tower)
  "Render a text representation of an editing tower in a buffer."
  (interactive)
  (let ((inherited-ground-buffer (eem--get-ground-buffer))
        (buffer (my-new-empty-buffer (eem--buffer-name tower)
                                     #'epistemic-meta-mode)))
    (with-current-buffer buffer
      ;; ground buffer is inherited from the original
      ;; to define the chain of reference
      (setq eem--ground-buffer
            inherited-ground-buffer)
      (eem--set-meta-buffer-appearance)
      (insert (eem-serialize-tower tower))
      (eem--enter-appropriate-mode))
    buffer))

(defun my-enter-tower-mode ()
  "Enter a buffer containing a textual representation of the
initial epistemic tower."
  (interactive)
  (dolist (tower (editing-ensemble-members eem--complex))
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
  (let ((ref-buf (eem--get-ground-buffer)))
    (with-current-buffer ref-buf
      (setq eem--last-tower-index eem--tower-index-on-entry))
    (eem--revert-ui)
    (kill-matching-buffers (concat "^" eem-buffer-prefix) nil t)
    (switch-to-buffer ref-buf)))

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

(provide 'eem-tower-mode)
