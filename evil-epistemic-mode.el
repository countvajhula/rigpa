;;; evil-epistemic-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/epistemic-mode
;; Version: 0.1
;; Keywords: evil

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Epistemic mode allows you to construct "towers" of editing modes
;; (in principle, these could be any evil modes) and manipulate which
;; tower is active at any point in time.  It generalizes both vim's
;; notion of editing mode, as well as Emacs's notion of a major mode
;; into a unified way of looking at things, that is, as towers of
;; modes which can be swapped and themselves edited using the very
;; modes they contain.
;;
;; In addition, epistemic mode also defines conventions that modes that
;; are "epistemic" should follow in order to be seamlessly integrated
;; into editing towers.  This includes conventions around keybindings
;; for moving up and down the hierarchy of editing levels, standard
;; semantics of modifier keys, defining a canonical action for each
;; mode, and other such conventions to ensure semantic uniformity across
;; editing levels.

;;; Code:

;; Define evil states for each epistemic mode
(evil-define-state mode
  "Mode state."
  :tag " <M> "
  :message "-- MODE --"
  :enable (normal))

(require 'eem-char-mode)
(require 'eem-word-mode)
(require 'eem-line-mode)
(require 'symex)
(require 'eem-view-mode)
(require 'eem-window-mode)
(require 'eem-file-mode)
(require 'eem-buffer-mode)
(require 'eem-system-mode)
(require 'eem-application-mode)
(require 'eem-activity-mode)
(require 'eem-text-mode)

;; define face for use in epistemic mode
(make-face 'eem-face)
(set-face-font 'eem-face "-*-Consolas-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-foreground 'eem-face "tomato")

(setq eem-complete-tower
      (ht ('name "complete")
          ('levels (list (ht ('name "insert")
                             ('mode-entry 'evil-insert-state))
                         (ht ('name "char")
                             ('mode-entry 'evil-char-state))
                         (ht ('name "word")
                             ('mode-entry 'evil-word-state))
                         (ht ('name "line")
                             ('mode-entry 'evil-line-state))
                         (ht ('name "activity")
                             ('mode-entry 'evil-activity-state))
                         (ht ('name "normal")
                             ('mode-entry 'evil-normal-state))
                         (ht ('name "view")
                             ('mode-entry 'evil-view-state))
                         (ht ('name "window")
                             ('mode-entry 'evil-window-state))
                         (ht ('name "file")
                             ('mode-entry 'evil-file-state))
                         (ht ('name "buffer")
                             ('mode-entry 'evil-buffer-state))
                         (ht ('name "system")
                             ('mode-entry 'evil-system-state))
                         (ht ('name "application")
                             ('mode-entry 'evil-application-state))))))

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
                             ('mode-entry 'evil-symex-state))
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

(defun eem-enter-mode-with-recall (mode)
  "Enter MODE, but remember the previous state to return to it."
  (interactive)
  (let* ((mode-name (symbol-name mode))
         (recall (intern (concat "evil-" (symbol-name evil-state) "-state"))))
    (setq-local eem-recall recall)
    (funcall (intern (concat "evil-" mode-name "-state")))))

(defun eem-exit-mode-with-recall (mode)
  "Exit MODE to a prior state, unless it has already exited to another state."
  (interactive)
  (if (equal evil-state mode)
      (let ((recall (and (boundp 'eem-recall)
                         eem-recall)))
        (if recall
            (progn (setq-local eem-recall nil)
                   (funcall recall))
          ;; TODO: make interop to a sane "normal"
          (evil-normal-state)))
    ;; in either case null out the recall
    (setq-local eem-recall nil)))

(defun eem--set-mode-exit-flag (mode)
  "Set a mode exit flag to indicate cleanup operations need to be performed."
  (let* ((mode-name (symbol-name mode))
         (hydra (intern (concat "hydra-" mode-name))))
    (hydra-set-property hydra :exiting t)))

(defun eem--exit-mode (mode)
  "Exit a mode and perform any cleanup."
  (let* ((mode-name (symbol-name mode))
         (hydra (intern (concat "hydra-" mode-name))))
    (when (hydra-get-property hydra :exiting)
      (eem-exit-mode-with-recall mode)
      (hydra-set-property hydra :exiting nil))))

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
(global-set-key (kbd "H-<escape>") 'evil-force-normal-state)

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
  ("s-m" eem-flashback-to-last-tower "flashback" :exit t)  ; canonical action
  ("<return>" eem-enter-selected-level "enter selected level" :exit t)
  ("s-<return>" eem-enter-selected-level "enter selected level" :exit t)
  ("i" my-noop "exit" :exit t)
  ("<escape>" nil "exit" :exit t))
  ;("s-<return>" eem-enter-lower-level "enter lower level" :exit t)
  ;("s-<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(provide 'evil-epistemic-mode)
;;; evil-epistemic-mode.el ends here
