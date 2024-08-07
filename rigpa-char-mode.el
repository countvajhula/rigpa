;;; rigpa-char-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/rigpa

;; This program is "part of the world," in the sense described at
;; http://drym.org.  From your perspective, this is no different than
;; MIT or BSD or other such "liberal" licenses that you may be
;; familiar with, that is to say, you are free to do whatever you like
;; with this program.  It is much more than BSD or MIT, however, in
;; that it isn't a license at all but an idea about the world and how
;; economic systems could be set up so that everyone wins.  Learn more
;; at drym.org.
;;
;; This work transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.
;;

;;; Commentary:
;;
;; A mode to refer to characters
;;

;;; Code:

(require 'evil)
(require 'chimera)
(require 'rigpa-evil-support)

(defvar rigpa-char-mode-map (make-sparse-keymap))

(define-minor-mode rigpa-char-mode
  "Minor mode to modulate keybindings in rigpa char mode."
  :lighter "char"
  :keymap rigpa-char-mode-map)

(evil-define-state char
  "Char state."
  :tag " <X> "
  :message "-- CHAR --"
  :enable (normal))

(defun rigpa-char-info ()
  "Info on character"
  (interactive)
  (what-cursor-position))

(evil-define-command rigpa-char-move-left (count)
  "Move character left."
  (interactive "p")
  (forward-char)
  (transpose-chars (- count))
  (backward-char))

(defun rigpa-char-move-left-more ()
  "Move character left more."
  (interactive)
  (rigpa-char-move-left 3))

(defun rigpa-char-move-left-most ()
  "Move character left most."
  (interactive)
  (evil-delete-char (point) (1+ (point)))
  (beginning-of-line)
  (evil-paste-before nil nil)
  (backward-char))

(evil-define-command rigpa-char-move-right (count)
  "Move character right."
  (interactive "p")
  (forward-char)
  (transpose-chars count)
  (backward-char))

(defun rigpa-char-move-right-more ()
  "Move character right more."
  (interactive)
  (rigpa-char-move-right 3))

(defun rigpa-char-move-right-most ()
  "Move character right most."
  (interactive)
  (evil-delete-char (point) (1+ (point)))
  (end-of-line)
  (evil-paste-after nil nil))

(evil-define-command rigpa-char-move-down (count)
  "Move character down."
  (interactive "p")
  (evil-delete-char (point) (1+ (point)))
  (evil-next-line count)
  (evil-paste-before nil nil)
  (backward-char))

(defun rigpa-char-move-down-more ()
  "Move character down more."
  (interactive)
  (rigpa-char-move-down 3))

(defun rigpa-char-move-down-most ()
  "Move character down most."
  (interactive)
  (let ((orig-column (current-column)))
    (evil-delete-char (point) (1+ (point)))
    (goto-char (nth 1 (evil-inner-paragraph)))
    (evil-previous-line)
    (evil-goto-column orig-column)
    (evil-paste-before nil nil)
    (backward-char)))

(evil-define-command rigpa-char-move-up (count)
  "Move character up."
  (interactive "p")
  (evil-delete-char (point) (1+ (point)))
  (evil-previous-line count)
  (evil-paste-before nil nil)
  (backward-char))

(defun rigpa-char-move-up-more ()
  "Move character up more."
  (interactive)
  (rigpa-char-move-up 3))

(defun rigpa-char-move-up-most ()
  "Move character up most."
  (interactive)
  (let ((orig-column (current-column)))
    (evil-delete-char (point) (1+ (point)))
    (goto-char (nth 0 (evil-inner-paragraph)))
    (evil-goto-column orig-column)
    (evil-paste-before nil nil)
    (backward-char)))

(defun rigpa-char-change ()
  "Change character"
  (interactive)
  (evil-substitute (point)
                   (+ (point) 1)
                   (quote exclusive)
                   nil))

(defun rigpa-char-toggle-case ()
  "Toggle case."
  (interactive)
  (evil-invert-char (point) (1+ (point))))

(evil-define-operator rigpa-char-yank (beg end type register yank-handler)
  "Yank (copy) character"
  :motion evil-forward-char
  (evil-yank beg end type register yank-handler))

(defvar rigpa--char-mode-keyspec
      '(("h" . evil-backward-char)
        ("j" . evil-next-line)
        ("k" . evil-previous-line)
        ("l" . evil-forward-char)
        ("c" . evil-substitute)
        ("y" . rigpa-char-yank)
        ("~" . rigpa-char-toggle-case)
        ("g" . goto-char) ; improve these to have beginning/end
        ("G" . goto-char) ; default behavior
        ("C-h" . (lambda ()
                   (interactive)
                   (evil-backward-char 3)))
        ("C-j" . (lambda ()
                   (interactive)
                   (evil-next-line 3)))
        ("C-k" . (lambda ()
                   (interactive)
                   (evil-previous-line 3)))
        ("C-l" . (lambda ()
                   (interactive)
                   (evil-forward-char 3)))
        ("M-h" . (lambda ()
                   (interactive)
                   (evil-beginning-of-line)))
        ("M-j" . (lambda ()
                   (interactive)
                   (evil-forward-paragraph)
                   (evil-previous-line)))
        ("M-k" . (lambda ()
                   (interactive)
                   (evil-backward-paragraph)
                   (evil-next-line)))
        ("M-l" . (lambda ()
                   (interactive)
                   (evil-end-of-line)))
        ("H" . rigpa-char-move-left)
        ("J" . rigpa-char-move-down)
        ("K" . rigpa-char-move-up)
        ("L" . rigpa-char-move-right)
        ("?" . rigpa-char-info)
        ("C-S-h" . rigpa-char-move-left-more)
        ("C-S-j" . rigpa-char-move-down-more)
        ("C-S-k" . rigpa-char-move-up-more)
        ("C-S-l" . rigpa-char-move-right-more)
        ("M-H" . rigpa-char-move-left-most)
        ("M-J" . rigpa-char-move-down-most)
        ("M-K" . rigpa-char-move-up-most)
        ("M-L" . rigpa-char-move-right-most))
      "Key specification for rigpa char mode.")

(rigpa--define-evil-keys-from-spec rigpa--char-mode-keyspec
                                   rigpa-char-mode-map
                                   'char)

(defvar chimera-char-mode-entry-hook nil
  "Entry hook for rigpa char mode.")

(defvar chimera-char-mode-exit-hook nil
  "Exit hook for rigpa char mode.")

(defun rigpa--on-char-mode-pre-entry ()
  "Enable char minor mode."
  (rigpa-char-mode 1))

(defun rigpa--on-char-mode-exit ()
  "Disable char minor mode."
  (rigpa-char-mode -1))

(defvar chimera-char-mode
  (make-chimera-mode :name "char"
                     :enter #'evil-char-state
                     :pre-entry-hook 'chimera-char-mode-entry-hook
                     :post-exit-hook 'chimera-char-mode-exit-hook
                     :entry-hook 'evil-char-state-entry-hook
                     :exit-hook 'evil-char-state-exit-hook))


(provide 'rigpa-char-mode)
;;; rigpa-char-mode.el ends here
