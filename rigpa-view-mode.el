;;; rigpa-view-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to your view of the buffer
;;

;;; Code:


;; a mode for navigating pages
;; TODO: region does not persist on entering mode, e.g. for
;;       use in "narrow" functionality
;; TODO: once inside view mode, shifting and then zooming
;;       should zoom into the new center of the screen, not
;;       the original cursor location (see below for more)

(require 'evil)
(require 'hydra)
(require 'chimera)
(require 'chimera-hydra)

(defvar-local rigpa-view--original-position nil)
;; TODO: support mark and recall
(defvar rigpa-view-preferred-zoom-level 40) ; make a defcustom
(defvar rigpa-view-preferred-zoom-level-tolerance 5)

(evil-define-state view
  "View state."
  :tag " <V> "
  :message "-- VIEW --"
  :enable (normal))

(defun rigpa-view-scroll-down ()
  (interactive)
  (evil-scroll-line-down 3))

(defun rigpa-view-scroll-up ()
  (interactive)
  (evil-scroll-line-up 3))

(defun rigpa-view-scroll-skip-up ()
  (interactive)
  (evil-scroll-line-up 9))

(defun rigpa-view-scroll-skip-down ()
  (interactive)
  (evil-scroll-line-down 9))

(defun rigpa-view-zoom-in ()
  "Zoom in"
  (interactive)
  (text-scale-increase 1)
  (recenter))

(defun rigpa-view-zoom-out ()
  "Zoom out"
  (interactive)
  (text-scale-decrease 1)
  (recenter))

(defun rigpa-view-reset-zoom ()
  "Reset zoom level to default"
  (interactive)
  (text-scale-adjust 0)
  (recenter))

(defun rigpa-view-reset-preferred-zoom ()
  "Reset zoom level to preferred"
  (interactive)
  ;; TODO: potential for infinite loop if zooming in/out takes leaps
  ;; that are too large. looks like text-scale can be changed by
  ;; non-integer values
  ;; TODO: don't zoom in if > N% of lines overflow -- use the logic
  ;; from Window mode
  ;; TODO: zoom should be stable
  (let ((min-zoom (- rigpa-view-preferred-zoom-level
                     rigpa-view-preferred-zoom-level-tolerance))
        (max-zoom (+ rigpa-view-preferred-zoom-level
                     rigpa-view-preferred-zoom-level-tolerance)))
    (cond ((< (window-screen-lines) min-zoom)
           (while (< (window-screen-lines) min-zoom)
             (text-scale-decrease 1)))
          ((> (window-screen-lines) max-zoom)
           (while (> (window-screen-lines) max-zoom)
             (text-scale-increase 1)))
          ;; otherwise do nothing
          (t nil)))
  (recenter))

(defun rigpa-view-scroll-left (&optional superlative)
  "Scroll view left"
  (interactive)
  (let ((n (cond ((eq superlative nil) 3)
                 ((eq superlative 'less) 1)
                 ((eq superlative 'more) 10))))
    (scroll-right n)))

(defun rigpa-view-scroll-right (&optional superlative)
  "Scroll view right"
  (interactive)
  (let ((n (cond ((eq superlative nil) 3)
                 ((eq superlative 'less) 1)
                 ((eq superlative 'more) 10))))
    (scroll-left n)))

(defun rigpa-view-recenter-at-top ()
  "Recenter view so that selected line is at the top"
  (interactive)
  (let ((this-scroll-margin
         (min (max 0 scroll-margin)
              (truncate (/ (window-body-height) 4.0)))))
    (recenter this-scroll-margin)))

(defun rigpa-view-recenter-at-bottom ()
  "Recenter view so that selected line is at the bottom"
  (interactive)
  (let ((this-scroll-margin
         (min (max 0 scroll-margin)
              (truncate (/ (window-body-height) 4.0)))))
    (recenter (- -1 this-scroll-margin))))

(defun rigpa-view-narrow ()
  "Narrow view to definition or region."
  (interactive)
  (if mark-active
      (narrow-to-region (region-beginning) (region-end))
    (narrow-to-defun)))

(defun rigpa-view-return-to-original-position ()
  "Return to position prior to view mode entry."
  (interactive)
  (if (pos-visible-in-window-p rigpa-view--original-position)
      (goto-char rigpa-view--original-position)
    (goto-char rigpa-view--original-position)
    (recenter)))


(defhydra hydra-view (:columns 5
                      :body-pre (chimera-hydra-signal-entry chimera-view-mode)
                      :post (chimera-hydra-portend-exit chimera-view-mode t)
                      :after-exit (chimera-hydra-signal-exit chimera-view-mode
                                                             #'chimera-handle-hydra-exit))
  "View mode"
  ("j" rigpa-view-scroll-down "down")
  ("k" rigpa-view-scroll-up "up")
  ("C-S-j" evil-scroll-line-down "down fine")
  ("C-S-k" evil-scroll-line-up "up fine")
  ("b" evil-scroll-page-up "page up")
  ("f" evil-scroll-page-down "page down")
  ("h" rigpa-view-scroll-left "scroll left")
  ("l" rigpa-view-scroll-right "scroll right")
  ("C-h" (lambda ()
           (interactive)
           (rigpa-view-scroll-left 'more)) "scroll left more")
  ("C-l" (lambda ()
           (interactive)
           (rigpa-view-scroll-right 'more)) "scroll right more")
  ("C-S-h" (lambda ()
             (interactive)
             (rigpa-view-scroll-left 'less)) "scroll left less")
  ("C-S-l" (lambda ()
             (interactive)
             (rigpa-view-scroll-right 'less)) "scroll right less")
  ("g" evil-goto-first-line "beginning")
  ("0" evil-goto-first-line "beginning")
  ("M-k" evil-goto-first-line "beginning")
  ("G" evil-goto-line "end")
  ("$" evil-goto-line "end")
  ("M-j" evil-goto-line "end")
  ("s-v" recenter "recenter" :exit t)
  ("v" recenter "recenter")
  ("C-k" rigpa-view-scroll-skip-up "skip up")
  ("C-j" rigpa-view-scroll-skip-down "skip down")
  ("H" rigpa-view-recenter-at-top "recenter at top")
  ("L" rigpa-view-recenter-at-bottom "recenter at bottom")
  ("<backspace>" rigpa-view-reset-zoom "reset zoom")
  ("=" rigpa-view-reset-preferred-zoom "reset to preferred")
  ("<tab>" rigpa-view-reset-preferred-zoom "reset to preferred")
  ("K" rigpa-view-zoom-in "zoom in")
  ("J" rigpa-view-zoom-out "zoom out")
  ("u" evil-scroll-up "leap up")
  ("d" evil-scroll-down "leap down")
  ("n" rigpa-view-narrow "narrow context")
  ("w" widen "widen to full view")
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("i" nil "exit" :exit t)
  ("q" rigpa-view-return-to-original-position "exit" :exit t)
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-view-mode-entry-hook nil
  "Entry hook for rigpa view mode.")

(defvar chimera-view-mode-exit-hook nil
  "Exit hook for rigpa view mode.")

(defun rigpa--on-view-mode-entry ()
  "Actions to take upon entry into view mode."
  (setq rigpa-view--original-position (point))
  ;; TODO: retain original point position but for the purposes of the view
  ;; consider the midpoint of the current view as the reference point
  ;; then, upon exit, if the original location is still visible, preserve it
  ;; otherwise select the center (this logic is already implemented for "quit"
  ;; and should be reused)
  ;; (move-to-window-line nil)
  ;; currently, zooming past a certain level causes original point to
  ;; "drag" view there
  (blink-cursor-mode -1)
  (internal-show-cursor nil nil))

(defun rigpa--on-view-mode-post-exit ()
  "Actions to take upon exit from view mode."
  (blink-cursor-mode 1) ; TODO: depend on user config instead
  (internal-show-cursor nil t)
  (if (pos-visible-in-window-p rigpa-view--original-position)
      (goto-char rigpa-view--original-position)
    (evil-window-middle)))

(defvar chimera-view-mode
  (make-chimera-mode :name "view"
                     :enter #'hydra-view/body
                     :pre-entry-hook 'chimera-view-mode-entry-hook
                     :post-exit-hook 'chimera-view-mode-exit-hook
                     :entry-hook 'evil-view-state-entry-hook
                     :exit-hook 'evil-view-state-exit-hook))

;; mark view navigations as not repeatable from the perspective
;; of Evil's dot operator - i.e. I believe these won't get added
;; to the repeat stack.
;; TODO: make this part of a generic "mode setup"
;; but is there a better way than manually registering all
;; interactive commands this way?
(mapc #'evil-declare-abort-repeat
      '(rigpa-view-scroll-up
        rigpa-view-scroll-down
        rigpa-view-scroll-skip-up
        rigpa-view-scroll-skip-down))


(provide 'rigpa-view-mode)
;;; rigpa-view-mode.el ends here
