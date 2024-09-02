;;; rigpa-buffer-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to buffers
;;

;;; Code:


(require 'cl-lib)
(require 'evil)
(require 'ivy)
(require 'chimera)
(require 's)
(require 'dynaring)
(require 'buffer-ring)
(require 'lithium)
(require 'rigpa-util)

(defconst rigpa-buffer-ring-name-prefix "rigpa-buffer-ring")

(defgroup rigpa-buffer nil
  "Rigpa buffer mode."
  :group 'rigpa)

(defcustom rigpa-buffer-ignore-buffers nil
  "Buffers to ignore in navigation."
  :type 'list
  :group 'rigpa-buffer)

(evil-define-state buffer
  "Buffer state."
  :tag " <B> "
  :message "-- BUFFER --")

(cl-defun rigpa-buffer-create (&optional
                               buffer-name
                               major-mode-to-use
                               &key
                               switch-p)
  "Create a new empty buffer.

If BUFFER-NAME is not provided, the new buffer will be named
“untitled” or “untitled<2>”, “untitled<3>”, etc. The buffer will be
created in the currently active (at the time of command execution)
major mode.
If SWITCH-P is true, switch to the newly created buffer.

Modified from:
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let* ((buffer-name (or buffer-name "untitled"))
         (major-mode-to-use (or major-mode-to-use major-mode))
         ($buf (generate-new-buffer buffer-name)))
    (with-current-buffer $buf
      (funcall major-mode-to-use)
      (setq buffer-offer-save t))
    (when switch-p
      (switch-to-buffer $buf))
    $buf))

(defun rigpa-buffer--count-lines-page ()
  "Modified from emacs's built-in count-lines-page to return a list of
   values corresponding to the position in the page."
  (interactive)
  (save-excursion
    (let ((opoint (point)) beg end
	  total before after)
      (forward-page)
      (beginning-of-line)
      (or (looking-at page-delimiter)
	  (end-of-line))
      (setq end (point))
      (backward-page)
      (setq beg (point))
      (setq total (count-lines beg end)
	    before (count-lines beg opoint)
	    after (count-lines opoint end))
      (list total before after))))

(defun rigpa-buffer-info ()
  "get info on current buffer -- similar to Vim's C-g"
  (interactive)
  (-let (((total before after) (rigpa-buffer--count-lines-page))
         bufinfo percentage page-position total-lines)
    (if (= total 0)
        (setq bufinfo (list "-- No lines in buffer --"))
      (setq percentage (floor (* (/ (float before)
                                    total)
                                 100)))
      (setq page-position (concat
                           "-- "
                           (number-to-string percentage)
                           "%"
                           " --"))
      (setq total-lines (concat
				         (number-to-string total)
				         " lines"))
	  (setq bufinfo (list total-lines page-position)))
    (push (buffer-file-name) bufinfo)
    (message "%s" (string-join bufinfo " "))))

(defun rigpa-buffer-set-mark (mark-name)
  "Set a mark"
  (interactive "cMark name?")
  (puthash mark-name (current-buffer) rigpa-buffer-marks-hash)
  (message "Mark '%c' set." mark-name))

(defun rigpa-buffer-get-mark (mark-name)
  "Retrieve a mark"
  (gethash mark-name rigpa-buffer-marks-hash))

(defun rigpa-buffer-return-to-mark (mark-name)
  "Return to mark"
  (interactive "cMark name?")
  (condition-case nil
      (switch-to-buffer (rigpa-buffer-get-mark mark-name))
    (error (message "Buffer no longer exists!"))))

(defun rigpa-buffer-return-to-original ()
  "Return to the buffer we were in at the time of entering
buffer mode."
  (interactive)
  (condition-case nil
      (buffer-ring-rotate-to-buffer (rigpa-buffer-original-buffer))
    (error (message "Buffer no longer exists!"))))

(defun rigpa-buffer-link-to-original ()
  "Reinsert the exit buffer near the original head of the ring.

When we exit buffer mode, the buffer we exit into should be considered
proximate to the original buffer upon entry into buffer mode, not to
the buffers encountered in transit to this buffer. To retain this notion
of recency, we rotate the ring back to the original orientation and then
re-insert (i.e. \"break insert\") the exit buffer at that position."
  (interactive)
  (let ((exit-buffer (current-buffer))
        (original-buffer (rigpa-buffer-original-buffer)))
    (unless (eq exit-buffer original-buffer)
      ;; rotate the ring back so the original buffer is at head, and
      ;; then surface the exit buffer so it's proximate to the original
      (buffer-ring-rotate-to-buffer original-buffer)
      (buffer-ring-switch-to-buffer exit-buffer))))

(defun rigpa-buffer--active-buffers ()
  "Get active buffers."
  (let ((buffers (seq-reverse (buffer-list))))
    ;; reversed for consistency with next-buffer / prev-buffer directions
    (if (eq rigpa--complex rigpa-meta-tower-complex)
        (seq-filter (lambda (buf)
                      (s-starts-with-p rigpa-buffer-prefix (buffer-name buf)))
                    buffers)
      (seq-filter (lambda (buf)
                    ;; names of "invisible" buffers start with a space
                    ;; https://www.emacswiki.org/emacs/InvisibleBuffers
                    (and (not (s-starts-with-p " " (buffer-name buf)))
                         (not (member (buffer-name buf) rigpa-buffer-ignore-buffers))))
                  buffers))))

(defun rigpa-buffer--non-file-buffer-p (&optional buffer)
  "Predicate to check if BUFFER is one that isn't associated with a file.

This includes e.g. REPLs, the Messages buffer."
  ;; TODO: maybe a "process" category would make sense, i.e. a buffer
  ;; that has an associated process?
  (let ((buffer (buffer-ring--parse-buffer buffer)))
    (and (not (rigpa-buffer--read-only-buffer-p buffer))
         (string-match-p "^\*" (buffer-name buffer)))))

(defun rigpa-buffer--read-only-buffer-p (&optional buffer)
  "Predicate to check if BUFFER is read-only.

This includes e.g. the Messages buffer, the Magit status buffer,
but not REPLs and Scratch buffers."
  (let ((buffer (buffer-ring--parse-buffer buffer)))
    (with-current-buffer buffer
      buffer-read-only)))

(defun rigpa-buffer--typical-buffer-p (&optional buffer)
  "Predicate to check if BUFFER is \"typical.\"

This simply is the complement of read-only or non-file buffer
(see the other buffer predicates)."
  (let ((buffer (buffer-ring--parse-buffer buffer)))
    (and (not (rigpa-buffer--non-file-buffer-p buffer))
         (not (rigpa-buffer--read-only-buffer-p buffer)))))

(defun rigpa-buffer--refresh-ring (ring-name
                                   ring-membership-criterion-p
                                   active-buffers)
  "Create or update the buffers in ring RING-NAME."
  (let* ((ring-prefix (if (eq rigpa--complex rigpa-meta-tower-complex)
                          "2"
                        "0")) ; TODO: derive from coordinates later
         (ring-name (concat rigpa-buffer-ring-name-prefix
                            "-"
                            ring-prefix
                            "-"
                            ring-name))
         (ring-buffer-hash (make-hash-table :test #'equal)))
    ;; add any buffers in the current active list of buffers
    ;; to the buffer ring that aren't already there (e.g. buffers
    ;; created since the last entry into buffer mode). If this is
    ;; the first entry into buffer mode, create the buffer ring
    ;; from scratch with all of the currently active buffers
    (let ((ring-buffers (dynaring-values
                         (buffer-ring-ring-ring
                          (buffer-ring-torus-get-ring ring-name)))))
      (dolist (buf ring-buffers)
        (puthash (buffer-name buf) t ring-buffer-hash))
      (let ((fresh-buffers
             (seq-filter (lambda (buf)
                           (and (funcall ring-membership-criterion-p buf)
                                (not
                                 (gethash (buffer-name buf)
                                          ring-buffer-hash))))
                         active-buffers)))
        ;; we could just add all the buffers to the ring naively,
        ;; and that would be fine since buffer-ring takes no action
        ;; if the buffer happens to already be a member. But we don't
        ;; do that since each time this happens a message is echoed
        ;; to indicate that to the user, and the cost of I/O over
        ;; possibly hundreds of buffer additions could add a perceptible
        ;; lag in buffer mode entry. So we efficiently compute the
        ;; difference and just add those buffers
        (dolist (buf fresh-buffers)
          (buffer-ring-add ring-name buf))))))

(defun rigpa-buffer-refresh-ring ()
  "Create or update the buffer ring upon entry into buffer mode."
  (interactive)
  ;; activate buffer-ring minor mode if it isn't already active,
  ;; which ensures that hooks etc. are in place to keep buffers
  ;; and rings synchronized.
  (unless buffer-ring-mode
    (buffer-ring-mode))
  ;; add any buffers in the current active list of buffers
  ;; to the buffer ring that aren't already there (e.g. buffers
  ;; created since the last entry into buffer mode). If this is
  ;; the first entry into buffer mode, create the buffer ring
  ;; from scratch with all of the currently active buffers
  (let* ((active-buffers (rigpa-buffer--active-buffers))
         ;; TODO: would be better to do it as an assembly line
         ;; where buffers matching a predicate are removed from
         ;; the line so that there's no chance they'd be added
         ;; to a subsequent ring. At the moment each predicate
         ;; explicity checks that buffers also _don't_ match
         ;; preceding predicates.
         (rings (list (list "readonly" #'rigpa-buffer--read-only-buffer-p)
                      (list "special" #'rigpa-buffer--non-file-buffer-p)
                      (list "typical" #'rigpa-buffer--typical-buffer-p))))
    ;; we could just add all the buffers to the ring naively,
    ;; and that would be fine since buffer-ring takes no action
    ;; if the buffer happens to already be a member. But we don't
    ;; do that since each time this happens a message is echoed
    ;; to indicate that to the user, and the cost of I/O over
    ;; possibly hundreds of buffer additions could add a perceptible
    ;; lag in buffer mode entry. So we efficiently compute the
    ;; difference and just add those buffers
    (dolist (ring-config rings)
      (rigpa-buffer--refresh-ring (nth 0 ring-config)
                                  (nth 1 ring-config)
                                  active-buffers))))

(defun rigpa-buffer--setup-buffer-marks-table ()
  "Initialize the buffer marks hashtable and add an entry for the
current ('original') buffer."
  (interactive)
  (defvar rigpa-buffer-marks-hash
    (make-hash-table :test 'equal))
  (rigpa-buffer--save-original-buffer))

(defun rigpa-buffer--save-original-buffer ()
  "Save current buffer as original buffer."
  (interactive)
  (rigpa-buffer-set-mark ?0))

(defun rigpa-buffer-original-buffer ()
  "Get original buffer identifier"
  (interactive)
  (rigpa-buffer-get-mark ?0))

(defun rigpa-buffer-yank ()
  "Save current buffer identifier."
  (interactive)
  (rigpa-buffer-set-mark ?1))

(defun rigpa-buffer-paste ()
  "Return to yanked buffer."
  (interactive)
  (rigpa-buffer-return-to-mark ?1))

(defun rigpa-buffer-search ()
  "Search for buffer."
  (interactive)
  (rigpa-buffer-return-to-original)
  (ivy-switch-buffer))

(defun rigpa-buffer-alternate ()
  "Switch to most recent buffer."
  (interactive)
  ;; TODO: should ignore ring and just do MRU in current window?
  (let* ((ring (buffer-ring-ring-ring (buffer-ring-current-ring)))
         (other-buffer (dynaring-segment-value
                        (dynaring-segment-next (dynaring-head ring)))))
    ;; we can't simply rotate because we want to reinsert (i.e. "break insert")
    ;; it so that recency ordering reflects correctly
    (buffer-ring-switch-to-buffer other-buffer)))

(lithium-define-global-mode rigpa-buffer-mode
  "Buffer mode"
  (("s-b" rigpa-buffer-alternate t)
   ("b" rigpa-buffer-alternate t)
   ("h" buffer-ring-next-buffer)
   ("j" ignore)
   ("k" ignore)
   ("l" buffer-ring-prev-buffer)
   ("y" rigpa-buffer-yank)
   ("p" rigpa-buffer-paste)
   ("n" (lambda ()
          (interactive)
          (rigpa-buffer-create nil nil :switch-p t))
    t)
   ("m" rigpa-buffer-set-mark)
   ("'" rigpa-buffer-return-to-mark t)
   ("`" rigpa-buffer-return-to-mark t)
   ("s" rigpa-buffer-search t)
   ("/" rigpa-buffer-search t)
   ("i" ibuffer t)
   ("x" kill-buffer t)
   ("?" rigpa-buffer-info t)
   ("q" rigpa-buffer-return-to-original t)
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " buffer"
  :group 'rigpa)

(defvar chimera-buffer-mode-entry-hook nil
  "Entry hook for rigpa buffer mode.")

(defvar chimera-buffer-mode-exit-hook nil
  "Exit hook for rigpa buffer mode.")

(defun rigpa--on-buffer-mode-entry ()
  "Actions to take upon entry into buffer mode."
  (rigpa-buffer--setup-buffer-marks-table)
  (rigpa-buffer-refresh-ring)
  (let* ((ring-name (if (eq rigpa--complex rigpa-meta-tower-complex)
                        "2"
                      "0"))    ; TODO: derive from coordinates later
         (buffer-ring-name (concat rigpa-buffer-ring-name-prefix
                                   "-"
                                   ring-name
                                   (cond ((rigpa-buffer--read-only-buffer-p) "-readonly")
                                         ((rigpa-buffer--non-file-buffer-p) "-special")
                                         (t "-typical")))))
    ;; TODO: the appropriate ring is a function of both the current
    ;; meta level but also the current buffer.  we could probably
    ;; make this selection of appropriate ring more robust by using
    ;; something like bufler, whose categories could correspond to
    ;; distinct rings, and maybe meta level could simply be another
    ;; category here.
    (buffer-ring-torus-switch-to-ring buffer-ring-name))
  ;; TODO: probably do this via a standard internal
  ;; rigpa hook in mode registration
  (rigpa--for-all-buffers #'evil-buffer-state))

(defun rigpa--on-buffer-mode-exit ()
  "Actions to take upon exit from buffer mode."
  (rigpa-buffer-link-to-original))

(defun rigpa--on-buffer-mode-post-exit ()
  "Actions to take upon exit from buffer mode."
  ;; TODO: probably do this (entering appropriate mode in current and original buffer)
  ;; via a standard internal rigpa hook in mode registration.
  ;; we can enter appropriate in original if different from current buffer
  (rigpa--for-all-buffers #'rigpa--enter-appropriate-mode))

(defvar chimera-buffer-mode
  (make-chimera-mode :name "buffer"
                     :enter #'rigpa-buffer-mode-enter
                     :exit #'rigpa-buffer-mode-exit
                     :pre-entry-hook 'rigpa-buffer-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-buffer-mode-post-exit-hook
                     :entry-hook 'rigpa-buffer-mode-post-entry-hook
                     :exit-hook 'rigpa-buffer-mode-pre-exit-hook
                     :manage-hooks nil))


(provide 'rigpa-buffer-mode)
;;; rigpa-buffer-mode.el ends here
