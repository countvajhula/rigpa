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

;; the prefix that will be used in naming all buffers used
;; in epistemic mode representations
(setq eem-buffer-prefix "EPISTEMIC-META")

;; ideally, epistemic mode should be aware when any evil state is entered,
;; if that state is in the present tower. For now, just handle the specific
;; case of insert mode, since it's the one that's often entered using a
;; non-eem entry point
(add-hook 'evil-insert-state-entry-hook
          (lambda (&rest args)
            (setq eem--current-level 0)))

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

;; TODO: wrap all evil states in lithium modes and register them with epistemic mode
;; TODO: fix recall - probably reframe it in terms of exit and entry (and eliminate any
;; unnecessary ad hoc cases of entry to normal mode)

(provide 'evil-epistemic-mode)
;;; evil-epistemic-mode.el ends here
