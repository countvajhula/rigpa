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
(require 'eem-tab-mode)
(require 'eem-history-mode)
(require 'eem-tower-mode)

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

(defun eem--temp-setup-buffer-marks-table ()
  "Initialize the buffer marks hashtable and add an entry for the
current ('original') buffer."
  (interactive)
  (defvar eem--temp-buffer-marks-hash
    (make-hash-table :test 'equal))
  (save-original-buffer))

(defun eem--temp-save-original-buffer ()
  "Save current buffer as original buffer."
  (interactive)
  (puthash "0" (current-buffer)
           eem--temp-buffer-marks-hash))

(defun eem--temp-original-buffer ()
  "Get original buffer identifier"
  (interactive)
  (gethash "0" eem--temp-buffer-marks-hash))

(defun eem--temp-return-to-original-buffer ()
  "Return to the buffer we were in at the time of entering
buffer mode."
  (interactive)
  (switch-to-buffer (eem--temp-original-buffer)))

(defun eem-enter-mode-with-recall (mode)
  "Enter MODE, but remember the previous state to return to it."
  (interactive)
  (let* ((mode-name (symbol-name mode))
         ;; we're relying on the evil state here even though the
         ;; delegation is hydra -> evil. Probably introduce an
         ;; independent state variable, for which the evil state
         ;; variable can be treated as a proxy for now
         (recall (let ((state (symbol-name evil-state)))
                   (if (equal state "normal")
                       (intern (concat "evil-" state "-state"))
                     (intern (concat "hydra-" state "/body"))))))
    (eem--temp-setup-buffer-marks-table)
    (eem--temp-save-original-buffer)
    (setq-local eem-recall recall)
    (let ((mode-entry (if (member mode-name (list "normal" "insert"))
                          ;; handle the (at present, hypothetical) case of entry
                          ;; to a standard evil mode
                          ;; no hydra for standard evil modes
                          (intern (concat "evil-" mode-name "-state"))
                        (intern (concat "hydra-" mode-name "/body")))))
      (funcall mode-entry))))

(defun eem-exit-mode-with-recall (mode)
  "Exit MODE to a prior state, unless it has already exited to another state."
  (interactive)
  (progn (with-current-buffer (eem--temp-original-buffer)
           (let ((recall (and (boundp 'eem-recall)
                              eem-recall)))
             (if recall
                 (progn (funcall recall))
               ;; TODO: make interop to a sane "normal"
               (evil-normal-state))))
         (let ((recall (and (boundp 'eem-recall)
                            eem-recall)))
           (if recall
               (progn (setq-local eem-recall nil)
                      (funcall recall))
             ;; TODO: make interop to a sane "normal"
             (evil-normal-state)))))

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

(define-key evil-insert-state-map [escape] 'eem-enter-higher-level)
(define-key evil-normal-state-map [escape] 'eem-enter-higher-level)
(define-key evil-normal-state-map [return] 'eem-enter-lower-level)
(global-set-key (kbd "H-<escape>") 'evil-force-normal-state)

(defun eem-hide-menu ()
  "Hide current mode menu."
  (let ((current-mode-name (symbol-name evil-state)))
    (message "mode is %s" (concat "hydra-" current-mode-name))
    (hydra-set-property
     (intern (concat "hydra-"
                     current-mode-name))
     :verbosity 0)))

(defun eem-show-menu ()
  "Show current mode menu."
  (let ((current-mode-name (symbol-name evil-state)))
    (hydra-set-property
     (intern (concat "hydra-"
                     current-mode-name))
     :verbosity 2)))

(defun eem-toggle-menu ()
  "Show/hide the current mode menu.

Note that hiding the menu still retains the current editing mode,
and simply toggles whether the menu is visible or not."
  (interactive)
  (let ((current-mode-name (symbol-name evil-state)))
    (let ((visibility (hydra-get-property
                       (intern (concat "hydra-"
                                       current-mode-name))
                       :verbosity)))
      (if (> visibility 0)
          (eem-hide-menu)
        (eem-show-menu)))))

;; [ ] make the tower representation a list instead of a hash, and streamline
;; mode entry via an interface (which uses hydra)
;; [ ] move tower config (and keybinding config) out of epistemic mode and into init.d
;; [ ] move all require's to init.d - their point is to simply load those modes, so they are the same as other package-related config in init.d
;; [x] get it working with tower mode / mode mode / core refactor

(provide 'evil-epistemic-mode)
;;; evil-epistemic-mode.el ends here
