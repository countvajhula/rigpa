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
(require 'hydra)
(require 'ivy)
(require 'chimera)
(require 'chimera-hydra)
(require 's)

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
  :message "-- BUFFER --"
  :enable (normal))

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
      (switch-to-buffer (rigpa-buffer-original-buffer))
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
      (dynaring-rotate-until (buffer-ring-ring-ring (buffer-ring-current-ring))
                             #'dynaring-rotate-right
                             (lambda (buf)
                               (eq original-buffer buf)))
      (buffer-ring-surface-buffer exit-buffer))))

(defun rigpa-buffer--active-buffers ()
  "Get active buffers."
  (seq-reverse ; for consistency with next-buffer / prev-buffer directions
   (if (eq rigpa--complex rigpa-meta-tower-complex)
       (seq-filter (lambda (buf)
                     (s-starts-with-p rigpa-buffer-prefix (buffer-name buf)))
                   (buffer-list))
     (seq-filter (lambda (buf)
                   ;; names of "invisible" buffers start with a space
                   ;; https://www.emacswiki.org/emacs/InvisibleBuffers
                   (and (not (s-starts-with-p " " (buffer-name buf)))
                        (not (member (buffer-name buf) rigpa-buffer-ignore-buffers))))
                 (buffer-list)))))

(defun rigpa-buffer-create-ring ()
  "Create or update the buffer ring upon entry into buffer mode."
  (interactive)
  (let* ((ring-name (if (eq rigpa--complex rigpa-meta-tower-complex)
                        "2"
                      "0")) ; TODO: derive from coordinates later
         (buffer-ring-name (concat rigpa-buffer-ring-name-prefix
                                   "-"
                                   ring-name))
         (ring-buffer-hash (make-hash-table :test #'equal)))
    ;; add any buffers in the current active list of buffers
    ;; to the buffer ring that aren't already there (e.g. buffers
    ;; created since the last entry into buffer mode). If this is
    ;; the first entry into buffer mode, create the buffer ring
    ;; from scratch with all of the currently active buffers
    (let ((active-buffers (rigpa-buffer--active-buffers))
          (ring-buffers (dynaring-values
                         (buffer-ring-ring-ring
                          (buffer-ring-torus-get-ring buffer-ring-name)))))
      (dolist (buf ring-buffers)
        (puthash (buffer-name buf) t ring-buffer-hash))
      (let ((fresh-buffers
             (seq-filter (lambda (buf)
                           (not (gethash (buffer-name buf)
                                         ring-buffer-hash)))
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
          (buffer-ring-add buffer-ring-name buf))))))

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
  (let* ((ring (buffer-ring-ring-ring (buffer-ring-current-ring)))
         (other-buffer (dynaring-segment-value
                        (dynaring-segment-next (dynaring-head ring)))))
    ;; we can't simply rotate because we want to reinsert (i.e. "break insert")
    ;; it so that recency ordering reflects correctly
    (buffer-ring-switch-to-buffer other-buffer)))

(defhydra hydra-buffer (:columns 3
                        :body-pre (chimera-hydra-signal-entry chimera-buffer-mode)
                        :post (progn (rigpa-buffer-link-to-original)
                                     (chimera-hydra-portend-exit chimera-buffer-mode t))
                        :after-exit (chimera-hydra-signal-exit chimera-buffer-mode
                                                               #'chimera-handle-hydra-exit))
  "Buffer mode"
  ("s-b" rigpa-buffer-alternate "switch to last" :exit t)
  ("b" rigpa-buffer-alternate "switch to last" :exit t)
  ("h" buffer-ring-next-buffer "previous")
  ("j" ignore nil)
  ("k" ignore nil)
  ("l" buffer-ring-prev-buffer "next")
  ("y" rigpa-buffer-yank "yank")
  ("p" rigpa-buffer-paste "paste")
  ("n" (lambda ()
         (interactive)
         (rigpa-buffer-create nil nil :switch-p t))
   "new" :exit t)
  ("m" rigpa-buffer-set-mark "set mark")
  ("'" rigpa-buffer-return-to-mark "return to mark" :exit t)
  ("`" rigpa-buffer-return-to-mark "return to mark" :exit t)
  ("s" rigpa-buffer-search "search" :exit t)
  ("/" rigpa-buffer-search "search" :exit t)
  ("i" ibuffer "list (ibuffer)" :exit t)
  ("x" kill-buffer "delete" :exit t)
  ("?" rigpa-buffer-info "info" :exit t)
  ("q" rigpa-buffer-return-to-original "return to original" :exit t)
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-buffer-mode-entry-hook nil
  "Entry hook for rigpa buffer mode.")

(defvar chimera-buffer-mode-exit-hook nil
  "Exit hook for rigpa buffer mode.")

(defun rigpa-buffer-enter-mode ()
  "Enter buffer mode (idempotent)."
  (interactive)
  (rigpa-buffer--setup-buffer-marks-table)
  (rigpa-buffer-create-ring)
  (unless (chimera-hydra-is-active-p "buffer")
    (let* ((ring-name (if (eq rigpa--complex rigpa-meta-tower-complex)
                          "2"
                        "0"))    ; TODO: derive from coordinates later
           (buffer-ring-name (concat rigpa-buffer-ring-name-prefix
                                     "-"
                                     ring-name)))
      (buffer-ring-torus-switch-to-ring buffer-ring-name))
    (hydra-buffer/body)))

(defvar chimera-buffer-mode
  (make-chimera-mode :name "buffer"
                     :enter #'rigpa-buffer-enter-mode
                     :pre-entry-hook 'chimera-buffer-mode-entry-hook
                     :post-exit-hook 'chimera-buffer-mode-exit-hook
                     :entry-hook 'evil-buffer-state-entry-hook
                     :exit-hook 'evil-buffer-state-exit-hook))


(provide 'rigpa-buffer-mode)
;;; rigpa-buffer-mode.el ends here
