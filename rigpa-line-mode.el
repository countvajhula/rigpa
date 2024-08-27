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

;; TODO: disable cursor in line mode

(require 'evil)
(require 'chimera)

(defvar rigpa-line--column 1)

(evil-define-state line
  "Line state."
  :tag " <L> "
  :message "-- LINE --")

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

(evil-define-command rigpa-line-delete-backwards (count)
  "Delete previous line."
  (interactive "p")
  (save-excursion
    (let ((result (forward-line (- count))))
      (if (= 0 result)
          (kill-whole-line count)
        (error "beginning of buffer")))))

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
      (newline count)
    (save-excursion
      (beginning-of-line)
      (newline count))))

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

(defun rigpa-line-delete-remaining ()
  "Delete remaining lines."
  (interactive)
  (evil-delete (line-beginning-position) (point-max)))

(defun rigpa-line-change-remaining ()
  "Change remaining lines."
  (interactive)
  (rigpa-line-delete-remaining)
  (evil-insert-state))

;; TODO: this doesn't trigger feedback via evil-goggles
;; probably needs to be defined in a more idiomatic way
(evil-define-command rigpa-line-yank-remaining ()
  "Yank (copy) remaining lines."
  (interactive)
  (evil-yank (line-beginning-position) (point-max) 'line))

(defun rigpa-line-bottom ()
  (interactive)
  (evil-goto-line))

(defun rigpa-line-top ()
  (interactive)
  (evil-goto-line 1))

(defun rigpa-line-jump-down ()
  (interactive)
  (evil-next-line 9))

(defun rigpa-line-jump-up ()
  (interactive)
  (evil-previous-line 9))

(lithium-define-local-mode rigpa-line-mode
  "Line mode."
  (("h" evil-previous-line)
   ("j" evil-next-line)
   ("k" evil-previous-line)
   ("l" evil-next-line)
   ("C-j" rigpa-line-jump-down)
   ("C-k" rigpa-line-jump-up)
   ("M-h" rigpa-line-top)
   ("M-k" rigpa-line-top)
   ("0" rigpa-line-top)
   ("M-l" rigpa-line-bottom)
   ("M-j" rigpa-line-bottom)
   ("$" rigpa-line-bottom)
   ("H" rigpa-line-move-left)
   ("J" rigpa-line-move-down)
   ("K" rigpa-line-move-up)
   ("L" rigpa-line-move-right)
   ("<tab>" rigpa-line-indent)
   ("<backspace>" rigpa-line-clear)
   ("s-l" rigpa-line-indent)
   (">" evil-shift-right-line)
   ("<" evil-shift-left-line)
   ("M-H" rigpa-line-move-far-left)
   ("M-J" rigpa-line-move-very-bottom)
   ("M-K" rigpa-line-move-very-top)
   ("M-L" rigpa-line-move-far-right)
   ("x" rigpa-line-delete)
   ("X" rigpa-line-delete-backwards)
   ("c" rigpa-line-change)
   ("y" rigpa-line-yank)
   ("p" evil-paste-after)
   ("D" rigpa-line-delete-remaining)
   ("C" rigpa-line-change-remaining)
   ("Y" rigpa-line-yank-remaining)
   ("P" evil-paste-before)
   ("'" rigpa-line-flashback)
   ("s" rigpa-line-split)
   ("v" rigpa-line-pulverize)
   ("+" evil-open-above)
   ("i" evil-open-above)
   ("a" evil-open-below)
   ("n" rigpa-line-insert-newline)
   ("C-S-o" rigpa-line-append-newline)
   ("o" evil-join)
   ("O" rigpa-line-join-backwards)
   (";" rigpa-line-toggle-comment)
   ("u" undo-tree-undo) ; undo-only
   ("C-r" undo-tree-redo) ; undo-redo
   ("?" rigpa-line-info)
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " line"
  :group 'rigpa)

;; entry and post-exit state transitions
;; enter and exit functions
(defun rigpa--on-line-mode-exit ()
  "Disable line minor mode."
  (blink-cursor-mode 1) ; TODO: depend on user config instead
  (internal-show-cursor nil t)
  (hl-line-mode -1)
  (evil-goto-column rigpa-line--column))

(defun rigpa--on-line-mode-entry ()
  "Actions to take upon entering line mode."
  (blink-cursor-mode -1)
  (internal-show-cursor nil nil)
  (setq rigpa-line--column (current-column))
  (beginning-of-line)
  (hl-line-mode 1)
  (evil-line-state))

(defun rigpa--on-line-mode-post-exit ()
  "Actions to take upon exiting line mode."
  (rigpa--enter-appropriate-mode))

;; TODO: generate enter and exit as part of rigpa mode definition macros?
(defun rigpa-enter-line-mode ()
  "Enter line mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `exit' in the lithium mode-defining macro."
  (lithium-enter-mode 'rigpa-line-mode))

(defun rigpa-exit-line-mode ()
  "Exit line mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `enter' in the lithium mode-defining macro."
  (lithium-exit-mode 'rigpa-line-mode))

(defvar chimera-line-mode
  (make-chimera-mode :name "line"
                     :enter #'rigpa-enter-line-mode
                     :exit #'rigpa-exit-line-mode
                     :pre-entry-hook 'rigpa-line-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-line-mode-post-exit-hook
                     :entry-hook 'rigpa-line-mode-post-entry-hook
                     :exit-hook 'rigpa-line-mode-pre-exit-hook
                     :manage-hooks nil))

(provide 'rigpa-line-mode)
;;; rigpa-line-mode.el ends here
