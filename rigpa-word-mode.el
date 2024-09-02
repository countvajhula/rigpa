;;; rigpa-word-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to words
;;

;;; Code:

(require 'evil)
(require 'lithium)
(require 'chimera)

(evil-define-state word
  "Word state."
  :tag " <W> "
  :message "-- WORD --")

(evil-define-motion rigpa-word-backward (count)
  "Motion for moving backward by a word."
  :type exclusive
  (let ((count (or count 1)))
    (evil-backward-WORD-begin count)))

(evil-define-motion rigpa-word-forward (count)
  "Motion for moving forward by a word."
  :type exclusive
  (let ((count (or count 1)))
    (evil-forward-WORD-begin count)))

(defun rigpa-word--select-word ()
  "Select nearest word, going backwards if necessary."
  (let ((on-word-p (save-excursion
                       (let ((original-position (point)))
                         (evil-backward-WORD-begin)
                         (evil-forward-WORD-begin)
                         (= (point) original-position)))))
      (unless on-word-p
        (let ((original-line (line-number-at-pos)))
          (cond ((save-excursion (evil-backward-WORD-begin)
                                 (= (line-number-at-pos)
                                    original-line))
                 (evil-backward-WORD-begin))
                ((save-excursion (evil-forward-WORD-begin)
                                 (= (line-number-at-pos)
                                    original-line))
                 (evil-forward-WORD-begin)))))))

(evil-define-motion rigpa-word-up (count)
  "Motion for moving up by a word."
  :type exclusive
  (let ((count (or count 1)))
    (evil-previous-line count)
    (rigpa-word--select-word)))

(evil-define-motion rigpa-word-down (count)
  "Motion for moving down by a word."
  :type exclusive
  (let ((count (or count 1)))
    (evil-next-line count)
    (rigpa-word--select-word)))

(evil-define-operator rigpa-word-delete (beg end type register yank-handler)
  "Delete word."
  :motion rigpa-word-forward
  (evil-delete beg end type register yank-handler))

(evil-define-operator rigpa-word-change (beg end type register yank-handler)
  "Change word."
  :motion rigpa-word-forward
  (evil-change beg end type register yank-handler))

(evil-define-operator rigpa-word-toggle-case (beg end type register yank-handler)
  "Toggle case."
  :motion rigpa-word-forward
  (evil-invert-case beg end))

(evil-define-operator rigpa-word-upper-case (beg end type register yank-handler)
  "Make upper case."
  :motion rigpa-word-forward
  (evil-upcase beg end))

(evil-define-operator rigpa-word-lower-case (beg end type register yank-handler)
  "Make lower case."
  :motion rigpa-word-forward
  (evil-downcase beg end))

(evil-define-operator rigpa-word-title-case (beg end type register yank-handler)
  "Make title case."
  :motion rigpa-word-forward
  (capitalize-region beg end))

(evil-define-command rigpa-word-move-backward (count)
  "Move word backwards."
  (interactive "p")
  (dotimes (i count)
    (evil-inner-word)
    (evil-backward-WORD-begin)
    (transpose-words 1)
    (evil-backward-WORD-begin 2)))

(evil-define-command rigpa-word-move-forward (count)
  "Move word forward"
  (interactive "p")
  (dotimes (i count)
    (evil-forward-WORD-begin)
    (transpose-words 1)
    (evil-backward-WORD-begin)))

(evil-define-command rigpa-word-move-down (count)
  "Move word down"
  (interactive "p")
  (dotimes (i count)
    (evil-inner-word nil nil nil nil)
    (let ((word-end-position (point)))
      (evil-backward-WORD-begin nil)
      (evil-delete (point) word-end-position 'exclusive nil nil)
      (evil-next-line)
      (evil-paste-before nil nil))))

(evil-define-command rigpa-word-move-up (count)
  "Move word up"
  (interactive "p")
  (dotimes (i count)
    (evil-inner-word nil nil nil nil)
    (let ((word-end-position (point)))
      (evil-backward-WORD-begin nil)
      (evil-delete (point) word-end-position 'exclusive nil nil)
      (evil-previous-line)
      (evil-paste-before nil nil))))

(defun rigpa-word-split ()
  "Split word into characters on separate lines."
  (interactive)
  (with-undo-collapse
    (apply #'rigpa-word-delete (evil-inner-WORD))
    (evil-open-below 1)
    (evil-force-normal-state)
    (evil-paste-after nil nil)
    (evil-beginning-of-line)
    (while (not (eolp))
      (evil-forward-char)
      (newline)
      (evil-force-normal-state))))

(defun rigpa-word-delete-others ()
  "Delete other words in line."
  (interactive)
  (with-undo-collapse
    (apply #'rigpa-word-delete (evil-inner-WORD))
    (evil-open-below 1)
    (evil-force-normal-state)
    (evil-paste-after nil nil)
    (evil-previous-line)
    (rigpa-line-delete)))

(evil-define-command rigpa-word-rotate-chars-right (count)
  "Rotate characters to the right"
  (interactive "p")
  (dotimes (i count)
    (save-excursion
      (let* ((word-bounds (evil-inner-word))
             (word-start (nth 0 word-bounds))
             (word-end (nth 1 word-bounds)))
        (evil-delete-backward-char (- word-end 1)
                                   word-end
                                   'exclusive
                                   nil)
        (goto-char word-start)
        (evil-paste-before nil nil)))))

(evil-define-command rigpa-word-rotate-chars-left (count)
  "Rotate characters to the left"
  (interactive "p")
  (dotimes (i count)
    (save-excursion
      (let* ((word-bounds (evil-inner-word))
             (word-start (nth 0 word-bounds))
             (word-end (nth 1 word-bounds)))
        (evil-delete-char word-start
                          (+ word-start 1)
                          'exclusive
                          nil)
        (goto-char (- word-end 1))
        (evil-paste-before nil nil)))))

(evil-define-command rigpa-word-join-forwards (count)
  "Join words forwards."
  (interactive "p")
  (dotimes (i count)
    (save-excursion
      (evil-forward-WORD-end)
      (forward-char)
      (if (eolp)
          (progn (forward-line)
                 (beginning-of-line)
                 (delete-horizontal-space)
                 (apply #'evil-delete (evil-inner-WORD))
                 (forward-line -1)
                 (end-of-line)
                 (evil-paste-after 1))
        (delete-horizontal-space)))))

(evil-define-command rigpa-word-join-backwards (count)
  "Join words backwards."
  (interactive "p")
  (evil-backward-WORD-begin)
  (rigpa-word-join-forwards count))

(defun rigpa-word-scroll-jump-backward ()
  "Scroll jump back across words."
  (interactive)
  (rigpa-word-backward 3))

(defun rigpa-word-scroll-jump-forward ()
  "Scroll jump forward across words."
  (interactive)
  (rigpa-word-forward 3))

(defun rigpa-word-first-word ()
  "Jump backward to the first word in the paragraph."
  (interactive)
  (goto-char (nth 0 (evil-inner-paragraph))))

(defun rigpa-word-last-word ()
  "Jump forward to the last word in the paragraph."
  (interactive)
  (evil-forward-paragraph)
  (evil-backward-WORD-begin))

(defun rigpa-word-add-to-end ()
  "Add to the end of this word."
  (interactive)
  (evil-forward-WORD-end)
  (forward-char)
  (evil-insert-state))

(defun rigpa-word-add-after ()
  "Add a word after this one."
  (interactive)
  (evil-forward-WORD-begin)
  (insert " ")
  (backward-char)
  (evil-insert-state))

(defun rigpa-word-add-before ()
  "Add a word before this one."
  (interactive)
  (evil-backward-WORD-end)
  (forward-char)
  (insert " ")
  (evil-insert-state))

(defun rigpa-word-add-to-beginning ()
  "Add to the beginning of this word."
  (interactive)
  (evil-insert-state))

(evil-define-command rigpa-word-paste-after (count &optional register yank-handler)
  "Paste after word."
  (interactive "p")
  (evil-forward-WORD-end)
  (forward-char)
  (evil-paste-after count register yank-handler))

(lithium-define-local-mode rigpa-word-mode
  "Word mode."
  (("h" rigpa-word-backward)
   ("j" rigpa-word-down)
   ("k" rigpa-word-up)
   ("l" rigpa-word-forward)
   ("H" rigpa-word-move-backward)
   ("J" rigpa-word-move-down)
   ("K" rigpa-word-move-up)
   ("L" rigpa-word-move-forward)
   ("x" rigpa-word-delete)
   ("c" rigpa-word-change)
   ("~" rigpa-word-toggle-case)
   ("gU" rigpa-word-upper-case)
   ("gu" rigpa-word-lower-case)
   ("gt" rigpa-word-title-case)
   ("s" rigpa-word-split)
   ("s-o" rigpa-word-delete-others)
   ("C-S-k" rigpa-word-join-forwards)
   ("C-S-j" rigpa-word-join-backwards)
   ("C-S-h" rigpa-word-rotate-chars-left)
   ("C-S-l" rigpa-word-rotate-chars-right)
   ("C-h" rigpa-word-scroll-jump-backward)
   ("C-k" rigpa-word-scroll-jump-backward)
   ("C-j" rigpa-word-scroll-jump-forward)
   ("C-l" rigpa-word-scroll-jump-forward)
   ("M-h" rigpa-word-first-word)
   ("0" rigpa-word-first-word)
   ("M-l" rigpa-word-last-word)
   ("$" rigpa-word-last-word)
   ("a" rigpa-word-add-to-end)
   ("i" rigpa-word-add-to-beginning)
   ("A" rigpa-word-add-after)
   ("I" rigpa-word-add-before)
   ("p" rigpa-word-paste-after)
   ("?" dictionary-lookup-definition)
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " word"
  :group 'rigpa)

;; TODO: review these:
;; exiting keys: c, a, i, A, I, s-r (delete), s-o (delete others), ?, Esc, Ret
;; TODO: add a spell-correct verb, which magically fixes a word according to whatever suggestion.

(defun rigpa--on-word-mode-entry ()
  "Enable word minor mode."
  (evil-word-state))

(defun rigpa--on-word-mode-post-exit ()
  "Enable word minor mode."
  (rigpa--enter-appropriate-mode))

(defvar chimera-word-mode
  (make-chimera-mode :name "word"
                     :enter #'rigpa-word-mode-enter
                     :exit #'rigpa-word-mode-exit
                     :pre-entry-hook 'rigpa-word-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-word-mode-post-exit-hook
                     :entry-hook 'rigpa-word-mode-post-entry-hook
                     :exit-hook 'rigpa-word-mode-pre-exit-hook
                     :manage-hooks nil))


(provide 'rigpa-word-mode)
;;; rigpa-word-mode.el ends here
