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
;; TODO: the only point location that should be significant
;;       within View mode is the original one upon entry.
;;       Any "recentering" commands (e.g. v, s-v, H, L) should
;;       be in relation to that original point.

(require 'evil)
(require 'chimera)
(require 'lithium)

(defvar-local rigpa-view--original-position nil)
;; TODO: support mark and recall
(defvar rigpa-view-preferred-zoom-level 40) ; make a defcustom
(defvar rigpa-view-preferred-zoom-level-tolerance 5)

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

(defmacro rigpa-view--with-zoom-context (&rest body)
  "Set virtual point location for zoom purposes.

Point is reset upon View mode exit, so this is 'virtual' in the sense
that it is only in effect within View mode to indicate the position in
reference to which we are zooming."
  `(let ((orig-pt (point))
         (bob-visible (pos-visible-in-window-p (point-min)))
         (eob-visible (pos-visible-in-window-p (point-max))))
     (cond ((and bob-visible eob-visible) (move-to-window-line nil))
           (bob-visible (move-to-window-line 0))
           (eob-visible (move-to-window-line -1))
           (t (move-to-window-line nil)))
     ,@body
     (goto-char orig-pt)))

(defun rigpa-view-zoom-in ()
  "Zoom in"
  (interactive)
  (rigpa-view--with-zoom-context
   (text-scale-increase 1)
   (recenter)))

(defun rigpa-view-zoom-out ()
  "Zoom out"
  (interactive)
  (rigpa-view--with-zoom-context
   (text-scale-decrease 1)
   (recenter)))

(defun rigpa-view-reset-zoom ()
  "Reset zoom level to default"
  (interactive)
  (rigpa-view--with-zoom-context
   (text-scale-adjust 0)
   (recenter)))

(defun rigpa-view-reset-preferred-zoom ()
  "Reset zoom level to preferred"
  (interactive)
  ;; TODO: potential for infinite loop if zooming in/out takes leaps
  ;; that are too large. looks like text-scale can be changed by
  ;; non-integer values
  ;; TODO: don't zoom in if > N% of lines overflow -- use the logic
  ;; from Window mode
  ;; TODO: zoom should be stable
  (rigpa-view--with-zoom-context
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
   (recenter)))

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

(lithium-define-local-mode rigpa-view-mode
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
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " view"
  :group 'rigpa)

(defun rigpa--on-view-mode-entry ()
  "Actions to take upon entry into view mode."
  ;; remember original point position, but for the purposes of the
  ;; view we'll usually consider the midpoint of the current view as
  ;; the reference point.
  ;; later, upon exit, if the original location is still visible,
  ;; we'll preserve it otherwise we'll position point in the middle of
  ;; the view
  (setq rigpa-view--original-position (point))
  (blink-cursor-mode -1)
  (internal-show-cursor nil nil))

(defun rigpa--on-view-mode-post-exit ()
  "Actions to take upon exit from view mode."
  (blink-cursor-mode 1) ; TODO: depend on user config instead
  (internal-show-cursor nil t)
  (if (and rigpa-view--original-position
           (pos-visible-in-window-p rigpa-view--original-position))
      (goto-char rigpa-view--original-position)
    (evil-window-middle)))

(defvar chimera-view-mode
  (make-chimera-mode :name "view"
                     :enter #'rigpa-view-mode-enter
                     :exit #'rigpa-view-mode-exit
                     :pre-entry-hook 'rigpa-view-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-view-mode-post-exit-hook
                     :entry-hook 'rigpa-view-mode-post-entry-hook
                     :exit-hook 'rigpa-view-mode-pre-exit-hook
                     :manage-hooks nil))

(defun rigpa-view-initialize ()
  "Initialize View mode."
  ;; mark view navigations as not repeatable from the perspective
  ;; of Evil's dot operator - i.e. I believe these won't get added
  ;; to the repeat stack.
  ;; TODO: is there a better way than manually registering all
  ;; interactive commands this way?
  (mapc #'evil-declare-abort-repeat
        '(rigpa-view-scroll-up
          rigpa-view-scroll-down
          rigpa-view-scroll-skip-up
          rigpa-view-scroll-skip-down))
  (add-hook 'rigpa-view-mode-post-entry-hook
            #'rigpa--on-view-mode-entry)
  (add-hook 'rigpa-view-mode-post-exit-hook
            #'rigpa--on-view-mode-post-exit))


(provide 'rigpa-view-mode)
;;; rigpa-view-mode.el ends here
