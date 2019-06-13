;; Define evil states for each epistemic mode
(evil-define-state char
  "Char state."
  :tag " <X> "
  :message "-- CHAR --"
  ;;:cursor ;; inherit from normal
  ;;:entry-hook ;; potentially call the hydra here
  ;;:exit-hook ;; none
  ;;:suppress-keymap) ;; should be t, but probably inherits from normal
  :enable (motion normal))

(evil-define-state word
  "Word state."
  :tag " <W> "
  :message "-- WORD --"
  :enable (motion normal))

(evil-define-state line
  "Line state."
  :tag " <L> "
  :message "-- LINE --"
  :enable (motion normal))

(evil-define-state symex
  "Symex state."
  :tag " <λ> "
  :message "-- SYMEX --"
  :enable (motion normal))

(evil-define-state view
  "View state."
  :tag " <V> "
  :message "-- VIEW --"
  :enable (motion normal))

(evil-define-state window
  "Window state."
  :tag " <W> "
  :message "-- WINDOW --"
  :enable (motion normal))

(evil-define-state file
  "File state."
  :tag " <F> "
  :message "-- FILE --"
  :enable (motion normal))

(evil-define-state buffer
  "Buffer state."
  :tag " <B> "
  :message "-- BUFFER --"
  :enable (motion normal))

(evil-define-state application
  "Application state."
  :tag " <A> "
  :message "-- APPLICATION --"
  :enable (motion normal))

(evil-define-state system
  "System state."
  :tag " <S> "
  :message "-- SYSTEM --"
  :enable (motion normal))

(evil-define-state activity
  "Activity state."
  :tag " <A> "
  :message "-- ACTIVITY --"
  :enable (motion normal))

(evil-define-state mode
  "Mode state."
  :tag " <M> "
  :message "-- MODE --"
  :enable (motion normal))

(require 'my-char-mode)
(require 'my-word-mode)
(require 'my-line-mode)
(require 'symex-mode)
(require 'my-view-mode)
(require 'my-window-mode)
(require 'my-file-mode)
(require 'my-buffer-mode)
(require 'my-system-mode)
(require 'my-application-mode)
(require 'my-activity-mode)

;; define face for use in epistemic mode
(make-face 'eem-face)
(set-face-font 'eem-face "-*-Consolas-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-foreground 'eem-face "tomato")

(setq eem-complete-tower
      (ht ('name "complete")
          ('levels (list (ht ('name "insert")
                             ('mode-entry 'evil-insert-state))
                         (ht ('name "char")
                             ('mode-entry 'hydra-char/body))
                         (ht ('name "word")
                             ('mode-entry 'hydra-word/body))
                         (ht ('name "line")
                             ('mode-entry 'hydra-line/body))
                         (ht ('name "activity")
                             ('mode-entry 'hydra-activity/body))
                         (ht ('name "normal")
                             ('mode-entry 'evil-normal-state))
                         (ht ('name "view")
                             ('mode-entry 'hydra-view/body))
                         (ht ('name "window")
                             ('mode-entry 'hydra-window/body))
                         (ht ('name "file")
                             ('mode-entry 'hydra-file/body))
                         (ht ('name "buffer")
                             ('mode-entry 'hydra-buffer/body))
                         (ht ('name "system")
                             ('mode-entry 'hydra-system/body))
                         (ht ('name "application")
                             ('mode-entry 'hydra-application/body))))))

(setq eem-vim-tower
      (ht ('name "vim")
          ('levels (list (ht ('name "insert")
                             ('mode-entry 'evil-insert-state))
                         (ht ('name "normal")
                             ('mode-entry 'evil-normal-state))))))

(setq eem-emacs-tower
      (ht ('name "emacs")
          ('levels (list (ht ('name "emacs")
                             ('mode-entry 'evil-emacs-state))))))

(setq eem-lisp-tower
      (ht ('name "lisp")
          ('levels (list (ht ('name "insert")
                             ('mode-entry 'evil-insert-state))
                         (ht ('name "symex")
                             ('mode-entry 'hydra-symex/body))
                         (ht ('name "normal")
                             ('mode-entry 'evil-normal-state))))))

(setq eem-towers
      (list eem-vim-tower
            eem-complete-tower
            eem-lisp-tower
            eem-emacs-tower))

;; the prefix that will be used in naming all buffers used
;; in epistemic mode representations
(setq eem-buffer-prefix "EPISTEMIC")

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

;; ideally, epistemic mode should be aware when any evil state is entered,
;; if that state is in the present tower. For now, just handle the specific
;; case of insert mode, since it's the one that's often entered using a
;; non-eem entry point
(add-function :after (symbol-function 'evil-insert-state)
              (lambda (&rest args) (setq eem--current-level 0)))

;;;; set tower to lisp tower in all lisp modes (emulating major mode -- TODO: improve)
;; (defvar lisp-modes (list 'emacs-lisp-mode
;;                          'scheme-mode
;;                          'racket-mode))

;; (dolist (mode lisp-modes)
;;   (let ((hook (intern (concat (symbol-name mode)
;;                               "-hook"))))
;;     (add-hook hook (lambda (&rest)
;;                      (setq eem--current-tower-index 2)
;;                      (setq eem--current-level 2)))))

(defun eem--get-reference-buffer ()
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

(defun eem--buffer-name (tower)
  "Buffer name to use for a given tower."
  (concat eem-buffer-prefix "-" (ht-get tower 'name)))

(defun eem--set-buffer-appearance ()
  "Configure mode mode appearance."
  (buffer-face-set 'eem-face)
  (text-scale-set 5)
  ;;(setq cursor-type nil))
  (hl-line-mode)
  (blink-cursor-mode -1)
  (internal-show-cursor nil nil)
  (display-line-numbers-mode 'toggle))

(defun eem--revert-buffer-appearance ()
  "Revert buffer appearance to settings prior to entering mode mode."
  (hl-line-mode -1)
  (blink-cursor-mode 1))

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
         (let ((level (nth level-number
                           tower-levels)))
           (insert "|―――"
                   (number-to-string level-number)
                   "―――|"
                   " " (ht-get level
                               'name) "\n")))
       (my-delete-line)))
    tower-buffer))

(defun my-enter-mode-mode ()
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
  (evil-mode-state))

(defun my-exit-mode-mode ()
  "Exit mode mode."
  (interactive)
  (with-current-buffer eem--reference-buffer
    (setq eem--last-tower-index eem--tower-index-on-entry))
  (eem--revert-buffer-appearance)
  (evil-normal-state)
  (kill-matching-buffers (concat "^" eem-buffer-prefix) nil t))

(define-key evil-insert-state-map [escape] 'eem-enter-higher-level)
(define-key evil-normal-state-map [escape] 'eem-enter-higher-level)
(define-key evil-normal-state-map [return] 'eem-enter-lower-level)
(global-set-key (kbd "s-<escape>") 'evil-force-normal-state)

(defun eem--enter-level (level-number)
  "Enter level LEVEL-NUMBER"
  (let* ((tower (eem--current-tower))
         (levels (ht-get tower 'levels))
         (tower-height (length levels))
         (level-number (if (< level-number
                              tower-height)
                           level-number
                         (- tower-height 1))))
    (let ((level (nth level-number levels)))
      (funcall (ht-get level 'mode-entry))
      (setq eem--current-level level-number))))

(defun eem-enter-selected-level ()
  "Enter selected level"
  (interactive)
  (eem--enter-level eem--selected-level))

(defun enter-first-level ()
  "Enter epistemic modes at first level"
  (interactive)
  (evil-force-normal-state)
  ;; start at the lowest level
  (eem--enter-level 1))

(defun eem-enter-lower-level ()
  "Enter lower level."
  (interactive)
  (eem--enter-level (- eem--current-level
                       1)))

(defun eem-enter-higher-level ()
  "Enter higher level."
  (interactive)
  (eem--enter-level (+ eem--current-level
                       1)))

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

(defhydra hydra-mode (:idle 1.0
                      :columns 4
                      :body-pre (my-enter-mode-mode)
                      :post (my-exit-mode-mode))
  "Mode mode"
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
  ("s-m" eem-flashback-to-last-tower :exit t)  ; canonical action
  ("<return>" eem-enter-selected-level :exit t)
  ("i" my-noop "exit" :exit t)
  ("<escape>" nil "exit" :exit t))
  ;("s-<return>" eem-enter-lower-level "enter lower level" :exit t)
  ;("s-<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-m") 'hydra-mode/body)

(provide 'evil-epistemic-mode)
