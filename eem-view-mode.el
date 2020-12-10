;; a mode for navigating pages
;; TODO: region does not persist on entering mode, e.g. for
;;       use in "narrow" functionality

(require 'chimera)
(require 'chimera-hydra)

(evil-define-state view
  "View state."
  :tag " <V> "
  :message "-- VIEW --"
  :enable (normal))

(defun my-scroll-half-page-up ()
  (interactive)
  (evil-scroll-line-up (/ (window-total-height) 2)))

(defun my-scroll-half-page-down ()
  (interactive)
  (evil-scroll-line-down (/ (window-total-height) 2)))

(defun my-scroll-skip-up ()
  (interactive)
  (evil-scroll-line-up 9))

(defun my-scroll-skip-down ()
  (interactive)
  (evil-scroll-line-down 9))

(defun my-reset-zoom ()
  "Reset zoom level to default"
  (interactive)
  (text-scale-adjust 0))

(defun my-scroll-left (&optional superlative)
  "Scroll view left"
  (interactive)
  (let ((n (cond ((eq superlative nil) 3)
                 ((eq superlative 'less) 1)
                 ((eq superlative 'more) 10))))
    (scroll-right n)))

(defun my-scroll-right (&optional superlative)
  "Scroll view right"
  (interactive)
  (let ((n (cond ((eq superlative nil) 3)
                 ((eq superlative 'less) 1)
                 ((eq superlative 'more) 10))))
    (scroll-left n)))

(defun my-recenter-at-top ()
  "Recenter view so that selected line is at the top"
  (interactive)
  (let ((this-scroll-margin
         (min (max 0 scroll-margin)
              (truncate (/ (window-body-height) 4.0)))))
    (recenter this-scroll-margin)))

(defun my-recenter-at-bottom ()
  "Recenter view so that selected line is at the bottom"
  (interactive)
  (let ((this-scroll-margin
         (min (max 0 scroll-margin)
              (truncate (/ (window-body-height) 4.0)))))
    (recenter (- -1 this-scroll-margin))))

(defun my-narrow-to-defun-or-region ()
  "Narrow view to definition or region."
  (interactive)
  (if mark-active
      (narrow-to-region (region-beginning) (region-end))
    (narrow-to-defun)))


(defhydra hydra-view (:columns 5
                      :post (chimera-hydra-portend-exit chimera-view-mode t)
                      :after-exit (chimera-hydra-signal-exit chimera-view-mode
                                                             #'chimera-handle-hydra-exit))
  "View mode"
  ("j" my-scroll-down "down")
  ("k" my-scroll-up "up")
  ("C-S-j" evil-scroll-line-down "down fine")
  ("C-S-k" evil-scroll-line-up "up fine")
  ("b" evil-scroll-page-up "page up")
  ("f" evil-scroll-page-down "page down")
  ("h" my-scroll-left "scroll left")
  ("l" my-scroll-right "scroll right")
  ("C-h" (lambda ()
           (interactive)
           (my-scroll-left 'more)) "scroll left more")
  ("C-l" (lambda ()
           (interactive)
           (my-scroll-right 'more)) "scroll right more")
  ("C-S-h" (lambda ()
             (interactive)
             (my-scroll-left 'less)) "scroll left less")
  ("C-S-l" (lambda ()
             (interactive)
             (my-scroll-right 'less)) "scroll right less")
  ("g" evil-goto-first-line "beginning")
  ("0" evil-goto-first-line "beginning")
  ("M-k" evil-goto-first-line "beginning")
  ("G" evil-goto-line "end")
  ("$" evil-goto-line "end")
  ("M-j" evil-goto-line "end")
  ("s-v" recenter "recenter" :exit t)
  ("v" recenter "recenter")
  ("C-k" my-scroll-skip-up "skip up")
  ("C-j" my-scroll-skip-down "skip down")
  ("L" my-recenter-at-top "recenter at top")
  ("H" my-recenter-at-bottom "recenter at bottom")
  ("<backspace>" my-reset-zoom "reset zoom")
  ("=" my-reset-zoom "reset zoom")
  ("K" (lambda ()
         (interactive)
         (text-scale-increase 1)
         (recenter))
   "zoom in")
  ("J" (lambda ()
         (interactive)
         (text-scale-decrease 1)
         (recenter))
   "zoom out")
  ("u" my-scroll-half-page-up "leap up")
  ("d" my-scroll-half-page-down "leap down")
  ("n" my-narrow-to-defun-or-region "narrow context")
  ("w" widen "widen to full view")
  ("H-m" eem-toggle-menu "show/hide this menu")
  ("i" nil "exit" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-view-mode-entry-hook nil
  "Entry hook for rigpa view mode.")

(defvar chimera-view-mode-exit-hook nil
  "Exit hook for rigpa view mode.")

(defvar chimera-view-mode
  (make-chimera-mode :name "view"
                     :enter #'hydra-view/body
                     :entry-hook 'evil-view-state-entry-hook
                     :exit-hook 'evil-view-state-exit-hook))


(provide 'eem-view-mode)
