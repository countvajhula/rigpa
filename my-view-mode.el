;; a mode for navigating pages
;; TODO: region does not persist on entering mode, e.g. for
;;       use in "narrow" functionality

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

(defun my-scroll-left ()
  "Scroll view left"
  (interactive)
  (scroll-right 3))

(defun my-scroll-right ()
  "Scroll view right"
  (interactive)
  (scroll-left 3))

(defun my-recenter-at-top ()
  "Recenter view so that selected line is at the top"
  (interactive)
  (let ((this-scroll-margin
         (min (max 0 scroll-margin)
              (truncate (/ (window-body-height) 4.0)))))
    (recenter this-scroll-margin)))

(defun my-recenter-at-top ()
  "Recenter view so that selected line is at the top"
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


(defhydra hydra-view (:idle 1.0
                      :columns 6
                      :body-pre (evil-view-state))
  "View mode"
  ("j" my-scroll-down "down")
  ("k" my-scroll-up "up")
  ("C-S-j" evil-scroll-line-down "down fine")
  ("C-S-k" evil-scroll-line-up "up fine")
  ("b" evil-scroll-page-up "page up")
  ("f" evil-scroll-page-down "page down")
  ("h" my-scroll-left "scroll left")
  ("l" my-scroll-right "scroll right")
  ("g" evil-goto-first-line "beginning")
  ("0" evil-goto-first-line "beginning")
  ("G" evil-goto-line "end")
  ("$" evil-goto-line "end")
  ("s-v" recenter "recenter" :exit t)
  ("v" recenter "recenter")
  ("C-k" my-scroll-skip-up "skip up")
  ("C-j" my-scroll-skip-down "skip down")
  ("C-l" my-recenter-at-top "recenter at top")
  ("C-h" my-recenter-at-bottom "recenter at bottom")
  ("H" my-reset-zoom "reset zoom")
  ("L" my-reset-zoom "reset zoom")
  ("K" text-scale-increase "zoom in")
  ("J" text-scale-decrease "zoom out")
  ("u" my-scroll-half-page-up "leap up")
  ("d" my-scroll-half-page-down "leap down")
  ("n" my-narrow-to-defun-or-region "narrow context")
  ("w" widen "widen to full view")
  ("i" my-noop "exit" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-v") 'hydra-view/body)

(provide 'my-view-mode)
