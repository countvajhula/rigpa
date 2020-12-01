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

(require 'chimera)
(require 'eem-char-mode)
(require 'eem-word-mode)
(require 'eem-line-mode)
(require 'symex)
(require 'eem-symex-mode)
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

;; the prefix that will be used in naming all buffers used
;; in epistemic mode representations
(setq eem-buffer-prefix "EPISTEMIC-META")

(defun eem-hide-menu (mode-name)
  "Hide current mode menu."
  (unless (member mode-name chimera-evil-states)
    ;; only supported for hydra
    (let ((mode-hydra (intern (concat "hydra-" mode-name))))
      (hydra-set-property mode-hydra :verbosity 0))))

(defun eem-show-menu (mode-name)
  "Show current mode menu."
  (unless (member mode-name chimera-evil-states)
    ;; only supported for hydra
    (let ((mode-hydra (intern (concat "hydra-" mode-name))))
      (hydra-set-property mode-hydra :verbosity 2))))

(defun eem-toggle-menu ()
  "Show/hide the current mode menu.

Note that hiding the menu still retains the current editing mode,
and simply toggles whether the menu is visible or not."
  (interactive)
  (let* ((mode-name (symbol-name evil-state))
         (mode-hydra (intern (concat "hydra-" mode-name))))
    (let ((visibility (hydra-get-property mode-hydra :verbosity)))
      (if (or (eq nil visibility)
              (> visibility 0))
          (eem-hide-menu mode-name)
        (eem-show-menu mode-name)))))

(define-derived-mode epistemic-meta-mode
  text-mode "Meta"
  "Major mode for meta modes"
  (define-key epistemic-meta-mode-map (kbd "g r") 'eem--reload-tower))

(define-derived-mode epistemic-meta-tower-mode
  text-mode "Tower"
  "Major mode for meta modes"
  (define-key epistemic-meta-tower-mode-map (kbd "g r") 'eem--reload-tower))

;; wrap native evil states in chimera modes
(defvar chimera-normal-mode
  (make-chimera-mode :name "normal"
                     :enter #'evil-normal-state
                     :entry-hook 'evil-normal-state-entry-hook
                     :exit-hook 'evil-normal-state-exit-hook))

(defvar chimera-insert-mode
  (make-chimera-mode :name "insert"
                     :enter #'evil-insert-state
                     :entry-hook 'evil-insert-state-entry-hook
                     :exit-hook 'evil-insert-state-exit-hook))

(defvar chimera-emacs-mode
  (make-chimera-mode :name "emacs"
                     :enter #'evil-emacs-state
                     :entry-hook 'evil-emacs-state-entry-hook
                     :exit-hook 'evil-emacs-state-exit-hook))

(defvar chimera-visual-mode
  (make-chimera-mode :name "visual"
                     :enter #'evil-visual-state
                     :entry-hook 'evil-visual-state-entry-hook
                     :exit-hook 'evil-visual-state-exit-hook))

(defvar chimera-replace-mode
  (make-chimera-mode :name "replace"
                     :enter #'evil-replace-state
                     :entry-hook 'evil-replace-state-entry-hook
                     :exit-hook 'evil-replace-state-exit-hook))

(defun eem--enter-lower-or-pass-through ()
  "Enter a lower level, or pass through to underlying keymap."
  (interactive)
  (if buffer-read-only
      ;; ideally should find a way to "pass through"
      ;; to lower-priority keymaps instead of specifying
      ;; what should happen on a per-mode basis here
      (cond ((eq major-mode 'dired-mode)
             (dired-find-file))
            ((eq major-mode 'help-mode)
             (push-button))
            (t (eem-enter-lower-level)))
    (eem-enter-lower-level)))

(defun eem--integrate-evil ()
  "Map standard evil state entry and exit points so they're managed by epistemic."
  ;; evil interop keybindings
  (define-key evil-normal-state-map [escape] 'eem-enter-higher-level)
  (define-key evil-normal-state-map [return] 'eem--enter-lower-or-pass-through)
  (define-key evil-visual-state-map [escape] 'eem-enter-higher-level)
  (define-key evil-visual-state-map [return] 'eem-enter-lower-level)
  (define-key evil-replace-state-map [escape] 'eem-enter-higher-level)
  (define-key evil-replace-state-map [return] 'eem-enter-lower-level)
  (define-key evil-insert-state-map [escape] 'eem-enter-higher-level)
  ;; TODO: this keybinding should be dependent on whether there are any
  ;; other modes in the tower. If not, then this shouldn't be bound
  ;; IOW this keybinding (and some class of bindings more generally)
  ;; is tower-specific
  (define-key evil-emacs-state-map [escape] 'eem-enter-higher-level))

(defun eem--register-modes ()
  "Register the standard modes with the framework."
  ;; register evil chimera states with the epistemic framework
  (eem-register-mode chimera-normal-mode)
  (eem-register-mode chimera-insert-mode)
  (eem-register-mode chimera-emacs-mode)
  (eem-register-mode chimera-visual-mode)
  (eem-register-mode chimera-replace-mode)

  ;; register all the other modes
  (eem-register-mode chimera-application-mode)
  (eem-register-mode chimera-line-mode)
  (eem-register-mode chimera-view-mode)
  (eem-register-mode chimera-activity-mode)
  (eem-register-mode chimera-history-mode)
  (eem-register-mode chimera-tab-mode)
  (eem-register-mode chimera-word-mode)
  (eem-register-mode chimera-window-mode)
  (eem-register-mode chimera-char-mode)
  (eem-register-mode chimera-system-mode)
  (eem-register-mode chimera-buffer-mode)
  (eem-register-mode chimera-file-mode)
  (eem-register-mode chimera-text-mode)
  (eem-register-mode chimera-symex-mode))

(defun eem--create-editing-structures ()
  "Create standard editing structures."

  ;; towers in base editing levels
  (setq eem-complete-tower
        (make-editing-ensemble :name "complete"
                               :default "normal"
                               :members (list chimera-insert-mode
                                              chimera-char-mode
                                              chimera-word-mode
                                              chimera-line-mode
                                              chimera-activity-mode
                                              chimera-normal-mode
                                              chimera-view-mode
                                              chimera-window-mode
                                              chimera-file-mode
                                              chimera-buffer-mode
                                              chimera-system-mode
                                              chimera-application-mode)))

  (setq eem-vim-tower
        (make-editing-ensemble :name "vim"
                               :default "normal"
                               :members (list chimera-insert-mode
                                              chimera-normal-mode)))
  (setq eem-emacs-tower
        (make-editing-ensemble :name "emacs"
                               :default "emacs"
                               :members (list chimera-emacs-mode)))
  (setq eem-lisp-tower
        (make-editing-ensemble :name "lisp"
                               :default "symex"
                               :members (list chimera-insert-mode
                                              chimera-symex-mode
                                              chimera-normal-mode)))

  ;; complexes for base editing levels
  (setq eem-general-complex
        (make-editing-ensemble
         :name "general"
         :default "vim"
         :members (list eem-vim-tower
                        eem-complete-tower
                        eem-lisp-tower
                        eem-emacs-tower)))

  ;; towers and complexes for meta levels
  (setq eem-meta-mode-tower
        (make-editing-ensemble :name "meta-mode"
                               :default "line"
                               :members (list chimera-line-mode)))
  (setq eem-meta-complex
        (make-editing-ensemble
         :name "meta"
         :default "meta-mode"
         :members (list eem-meta-mode-tower)))

  (setq eem-meta-tower-mode-tower
        (make-editing-ensemble :name "meta-tower"
                               :default "buffer"
                               :members (list chimera-buffer-mode)))
  (setq eem-meta-tower-complex
        (make-editing-ensemble
         :name "meta-tower"
         :default "meta-tower"
         :members (list eem-meta-tower-mode-tower))))

(defun eem--provide-editing-structures ()
  "Register editing structures so they're used in relevant major modes."
  ;; change these to use a state struct instead of globals
  ;; that state struct must specify
  ;;  (1) the complex, as an object (this should take care of default tower
  ;;      so there's no need to set the tower index initially)
  ;;  (2) the tower index
  ;;  (3) the level index
  ;; and eventually make these "coordinates" generic
  (when (boundp 'symex-mode)
    (dolist (mode-name symex-lisp-modes)
      (let ((mode-hook (intern (concat (symbol-name mode-name)
                                       "-hook"))))
        (add-hook mode-hook (lambda ()
                              (setq eem--current-tower-index 2)
                              (setq eem--current-level 2))))))
  (add-hook 'epistemic-meta-mode-hook
            ;; TODO: dispatch here based on meta level. If the level
            ;; is 1 use line mode / buffers, if it's 2,
            ;; use buffer mode / perspectives?
            (lambda ()
              (setq eem--complex eem-meta-complex)))
  (add-hook 'epistemic-meta-tower-mode-hook
            ;; TODO: dispatch here based on meta level. If the level
            ;; is 1 use line mode / buffers, if it's 2,
            ;; use buffer mode / perspectives?
            (lambda ()
              (setq eem--complex eem-meta-tower-complex))))

(defun eem-initialize ()
  "Initialize epistemic mode."
  (interactive)
  (eem--register-modes)
  ;; should make this optional via a defcustom flag
  ;; or potentially even have it in a separate evil-adapter package
  (when (boundp 'evil-mode)
    (eem--integrate-evil))
  (eem--create-editing-structures)
  (eem--provide-editing-structures)
  (if (and (boundp 'epistemic-show-menus) epistemic-show-menus)
      (dolist (mode (ht-values eem-modes))
        (eem-show-menu (chimera-mode-name mode)))
    (dolist (mode (ht-values eem-modes))
      (eem-hide-menu (chimera-mode-name mode)))))

(defun eem-disable ()
  "Disable epistemic."
  (interactive)
  ;; TODO:
  ;; remap evil keybindings
  ;; unregister major mode hooks
  ;; unregister all modes (including esp evil states)
  nil)


(eem-initialize)

;; the editing complex to use in a buffer
;; by default this is general complex unless a more tailored one
;; has been set (e.g. via major mode hook)
(defvar-local eem--complex eem-general-complex)


(provide 'evil-epistemic-mode)
;;; evil-epistemic-mode.el ends here
