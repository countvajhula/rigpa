;;; rigpa-line-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to lines
;;

;;; Code:


;; other possibilities:
;; - take number as arg
;; - reflect as a single evil command
;; - rotate words right/left
;; - > indent
;; - delete other lines
;; - copy line
;; similarly for "region-mode", possibly by invoking multiple cursors

(require 'evil)
(require 'chimera)
(require 'rigpa-evil-support)

(defvar rigpa-line-mode-map (make-sparse-keymap))
(defvar rigpa-line--column 1)

(define-minor-mode rigpa-line-mode
  "Minor mode to modulate keybindings in rigpa line mode."
  :lighter "line"
  :keymap rigpa-line-mode-map)

(evil-define-state line
  "Line state."
  :tag " <L> "
  :message "-- LINE --"
  :enable (normal))

(evil-define-command rigpa-line-move-down (count)
  "Move line down"
  (interactive "p")
  (unless (save-excursion
            (end-of-line)
            (or (eobp)
                (save-excursion
                  (evil-next-line)
                  (eobp))))
    (evil-next-line)
    (transpose-lines count)
    (evil-previous-line)))

(evil-define-command rigpa-line-move-up (count)
  "Move line up"
  (interactive "p")
  (dotimes (i count)
    (unless (= 1 (line-number-at-pos))
      (transpose-lines 1)
      (evil-previous-line 2))))

(evil-define-command rigpa-line-move-left (count)
  "Move line left"
  (interactive "p")
  (save-excursion
    (let (starting-from)
      (evil-first-non-blank)
      (setq starting-from (- (point) count))
      (if (< starting-from
             (line-beginning-position))
          (setq starting-from (line-beginning-position)))
      (evil-delete-backward-char starting-from
                                 (point)
                                 'exclusive
                                 nil))))

(evil-define-command rigpa-line-move-right (count)
  "Move line right"
  (interactive "p")
  (save-excursion
    (evil-first-non-blank)
    (insert-char #x20 count)))

(defun rigpa-line-move-far-left ()
  "Move line far left"
  (interactive)
  (save-excursion
    (evil-first-non-blank)
    (evil-delete (line-beginning-position)
                 (point)
                 (quote exclusive)
                 nil
                 nil)))

(defun rigpa-line-move-far-right ()
  "Move line far right"
  (interactive)
  (save-excursion
    (evil-beginning-of-line)
    (unless (bobp)
      (evil-previous-line)
      (evil-first-non-blank)
      (let ((line-position (- (point)
                              (line-beginning-position))))
        (evil-next-line)
        (rigpa-line-move-far-left)
        (rigpa-line-move-right line-position)))))

(defun rigpa-line-move-very-bottom ()
  "Move line to bottom"
  (interactive)
  (evil-execute-in-normal-state)
  (execute-kbd-macro (kbd ":.m$")))

(defun rigpa-line-move-very-top ()
  "Move line to top"
  (interactive)
  (evil-execute-in-normal-state)
  (execute-kbd-macro (kbd ":.m0")))

(evil-define-operator rigpa-line-delete (beg end type register yank-handler)
  "Delete line."
  :motion evil-line
  (evil-delete beg end type register yank-handler))

(defun rigpa-line-flashback ()
  "Flashback to prev line"
  (interactive)
  (evil-goto-mark-line ?'))

(defun rigpa-line-split ()
  "Split line on word separators"
  (interactive)
  (with-undo-collapse
    (evil-beginning-of-line)
    (while (not (eolp))
      (unless (equal (- (line-end-position)
                        (line-beginning-position))
                     1)
        (evil-forward-word-end))
      (execute-kbd-macro (kbd "a"))
      (newline)
      (evil-force-normal-state))))

(defun rigpa-line-pulverize ()
  "Split on every character"
  (interactive)
  (evil-beginning-of-line)
  (while (not (eolp))
    (evil-forward-char)
    (newline)
    (evil-force-normal-state)))

(defun rigpa-line-info ()
  "Info about the line"
  (interactive)

  (defun line-length (n)
    "Length of the Nth line.
From: https://emacs.stackexchange.com/questions/17846/calculating-the-length-of-a-line-in-a-buffer"
    (save-excursion
      (goto-char (point-min))
      (if (zerop (forward-line (1- n)))
          (- (line-end-position)
             (line-beginning-position)))))
  (message "Line %d, length = %d"
           (line-number-at-pos)
           (line-length current-line-number)))

(evil-define-command rigpa-line-toggle-comment (count)
  "Comment / uncomment line"
  (interactive "p")
  (comment-line count))

(evil-define-command rigpa-line-clear (count)
  "Clear line"
  (interactive "p")
  (evil-delete (line-beginning-position) (line-end-position)))

(evil-define-operator rigpa-line-yank (beg end type register yank-handler)
  "Yank (copy) line"
  :motion evil-line
  (evil-yank-line beg end type register))

(evil-define-operator rigpa-line-change (beg end type register yank-handler)
  "Change line."
  :motion evil-line
  (evil-change beg end type register yank-handler))

(evil-define-operator rigpa-line-indent (beg end type register yank-handler)
  "Indent line"
  :motion evil-line
  (evil-indent beg end))

(evil-define-command rigpa-line-insert-newline (count)
  "Insert newline and reindent."
  (interactive "p")
  (if (bolp)
      (newline-and-indent count)
    (save-excursion
      (beginning-of-line)
      (newline-and-indent count))))

(evil-define-command rigpa-line-append-newline (count)
  "Append newline and reindent."
  (interactive "p")
  (save-excursion
    (forward-line)
    (newline-and-indent count)))

(evil-define-command rigpa-line-join-backwards (count)
  "Join lines backwards."
  (interactive "p")
  (save-excursion
    (evil-previous-line count)
    (let ((current-prefix-arg count))
      (call-interactively #'evil-join))))

(defun rigpa-line-top ()
  (interactive)
  (evil-goto-line))

(defun rigpa-line-bottom ()
  (interactive)
  (evil-goto-line 1))

(defun rigpa-line-jump-down ()
  (interactive)
  (evil-next-line 9))

(defun rigpa-line-jump-up ()
  (interactive)
  (evil-previous-line 9))

(defvar rigpa--line-mode-keyspec
  '(("h" . evil-previous-line)
    ("j" . evil-next-line)
    ("k" . evil-previous-line)
    ("l" . evil-next-line)
    ("C-j" . rigpa-line-jump-down)
    ("C-k" . rigpa-line-jump-up)
    ("M-j" . rigpa-line-top)
    ("M-k" . rigpa-line-bottom)
    ("H" . rigpa-line-move-left)
    ("J" . rigpa-line-move-down)
    ("K" . rigpa-line-move-up)
    ("L" . rigpa-line-move-right)
    ("<tab>" . rigpa-line-indent)
    ("<backspace>" . rigpa-line-clear)
    ("s-l" . rigpa-line-indent)
    (">" . evil-shift-right-line)
    ("<" . evil-shift-left-line)
    ("M-H" . rigpa-line-move-far-left)
    ("M-J" . rigpa-line-move-very-bottom)
    ("M-K" . rigpa-line-move-very-top)
    ("M-L" . rigpa-line-move-far-right)
    ("x" . rigpa-line-delete)
    ("c" . rigpa-line-change)
    ("y" . rigpa-line-yank)
    ("p" . evil-paste-after)
    ("P" . evil-paste-before)
    ("'" . rigpa-line-flashback)
    ("s" . rigpa-line-split)
    ("v" . rigpa-line-pulverize)
    ("+" . evil-open-above)
    ("i" . evil-open-above)
    ("a" . evil-open-below)
    ("n" . rigpa-line-insert-newline)
    ("C-S-o" . rigpa-line-append-newline)
    ("o" . evil-join)
    ("O" . rigpa-line-join-backwards)
    (";" . rigpa-line-toggle-comment)
    ("?" . rigpa-line-info))
  "Key specification for rigpa line mode.")

(rigpa--define-evil-keys-from-spec rigpa--line-mode-keyspec
                                   rigpa-line-mode-map
                                   'line)

(defvar chimera-line-mode-entry-hook nil
  "Entry hook for rigpa line mode.")

(defvar chimera-line-mode-exit-hook nil
  "Exit hook for rigpa line mode.")

(defun rigpa--enable-line-minor-mode ()
  "Enable line minor mode."
  ;; probably want to rename this function to reflect
  ;; its broader scope of line mode entry actions
  (rigpa-line-mode 1)
  (setq rigpa-line--column (current-column))
  (beginning-of-line)
  (hl-line-mode 1))

(defun rigpa--disable-line-minor-mode ()
  "Disable line minor mode."
  (when rigpa-line-mode
    (rigpa-line-mode -1)
    (hl-line-mode -1)
    (evil-goto-column rigpa-line--column)))

(defvar chimera-line-mode
  (make-chimera-mode :name "line"
                     :enter #'evil-line-state
                     :pre-entry-hook 'chimera-line-mode-entry-hook
                     :post-exit-hook 'chimera-line-mode-exit-hook
                     :entry-hook 'evil-line-state-entry-hook
                     :exit-hook 'evil-line-state-exit-hook))


(provide 'rigpa-line-mode)
;;; rigpa-line-mode.el ends here
