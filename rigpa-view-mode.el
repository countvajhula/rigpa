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
(require 'lithium)

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

(lithium-define-mode rigpa-view-mode
  "View mode"
  (("j" rigpa-view-scroll-down)
   ("k" rigpa-view-scroll-up)
   ("C-S-j" evil-scroll-line-down)
   ("C-S-k" evil-scroll-line-up)
   ("b" evil-scroll-page-up)
   ("f" evil-scroll-page-down)
   ("h" rigpa-view-scroll-left)
   ("l" rigpa-view-scroll-right)
   ("C-h" (lambda ()
            (interactive)
            (rigpa-view-scroll-left 'more)))
   ("C-l" (lambda ()
            (interactive)
            (rigpa-view-scroll-right 'more)))
   ("C-S-h" (lambda ()
              (interactive)
              (rigpa-view-scroll-left 'less)))
   ("C-S-l" (lambda ()
              (interactive)
              (rigpa-view-scroll-right 'less)))
   ("g" evil-goto-first-line)
   ("0" evil-goto-first-line)
   ("M-k" evil-goto-first-line)
   ("G" evil-goto-line)
   ("$" evil-goto-line)
   ("M-j" evil-goto-line)
   ("v" recenter)
   ("C-k" rigpa-view-scroll-skip-up)
   ("C-j" rigpa-view-scroll-skip-down)
   ("H" rigpa-view-recenter-at-top)
   ("L" rigpa-view-recenter-at-bottom)
   ("<backspace>" rigpa-view-reset-zoom)
   ("=" rigpa-view-reset-preferred-zoom)
   ("<tab>" rigpa-view-reset-preferred-zoom)
   ("K" rigpa-view-zoom-in)
   ("J" rigpa-view-zoom-out)
   ("u" evil-scroll-up)
   ("d" evil-scroll-down)
   ("n" rigpa-view-narrow)
   ("w" widen)
   ("s-v" recenter t)
   ("i" nil t)
   ("q" rigpa-view-return-to-original-position t)
   ("<return>" rigpa-enter-lower-level t)
   ("<escape>" rigpa-enter-higher-level t))
  :lighter " view"
  :group 'rigpa)

(defun rigpa-enter-view-mode ()
  "Enter view mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `exit' in the lithium mode-defining macro."
  (rigpa-view-mode 1))

(defun rigpa-exit-view-mode ()
  "Exit view mode.

We would prefer to have a thunk here so it's more easily usable with
hooks than anonymous lambdas. The minor mode function called without
arguments toggles rather than enters or exits, so this is more
explicit.

TODO: generate this and `enter' in the lithium mode-defining macro."
  (rigpa-view-mode -1))

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

(defun rigpa--on-view-mode-exit ()
  "Actions to take upon exit from view mode."
  (blink-cursor-mode 1) ; TODO: depend on user config instead
  (internal-show-cursor nil t)
  (if (pos-visible-in-window-p rigpa-view--original-position)
      (goto-char rigpa-view--original-position)
    (evil-window-middle)))

(defvar chimera-view-mode
  (make-chimera-mode :name "view"
                     :enter #'rigpa-enter-view-mode
                     :exit #'rigpa-exit-view-mode
                     :pre-entry-hook 'chimera-view-mode-entry-hook
                     :post-exit-hook 'chimera-view-mode-exit-hook
                     :entry-hook 'rigpa-view-mode-entry-hook
                     :exit-hook 'rigpa-view-mode-exit-hook))

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
