(defun current-transparency ()
  (nth 0
       (frame-parameter (selected-frame)
			'alpha)))

;; Set transparency of emacs
;; From: https://www.emacswiki.org/emacs/TransparentEmacs
(defun transparency (value)
 "Sets the transparency of the frame window. 0=transparent/100=opaque"
 (interactive "nTransparency Value 0 - 100 opaque:")
 (set-frame-parameter (selected-frame) 'alpha (cons value value)))

(defun adjust-transparency (delta)
  "Adjust the transparency of the frame window by the configured delta,
   in the range: 0=transparent/100=opaque"
  (interactive)
  (transparency (+ (current-transparency)
                   delta)))

(defun increase-transparency ()
  "Increase frame transparency."
  (interactive)
  (adjust-transparency -3))

(defun decrease-transparency ()
  "Decrease frame transparency."
  (interactive)
  (adjust-transparency 3))

(defun maximize-transparency ()
  "Maximize frame transparency (i.e. make transparent)"
  (interactive)
  (transparency 0))

(defun minimize-transparency ()
  "Minimize frame transparency (i.e. make opaque)"
  (interactive)
  (transparency 100))

(defun return-to-original-transparency ()
  "Return to original transparency prior to making changes."
  (interactive)
  (transparency original-transparency))

(defhydra hydra-transparency (:columns 1
                              :body-pre (setq original-transparency
                                              (current-transparency)))
  "Control frame transparency"
  ("+" decrease-transparency "decrease transparency")
  ("-" increase-transparency "increase transparency")
  ("k" decrease-transparency "decrease transparency")
  ("j" increase-transparency "increase transparency")
  ("K" minimize-transparency "min transparency (opaque)")
  ("J" maximize-transparency "max transparency (transparent)")
  ("q" return-to-original-transparency  "return to original transparency" :exit t)
  ("<escape>" my-noop "quit" :exit t))

(defhydra hydra-application (:columns 1
                             :exit t
                             :body-pre (evil-application-state))
  "Control application environment"
  ("t" hydra-transparency/body "transparency")
  ("n" display-line-numbers-mode "toggle line numbers")
  ("l" hl-line-mode "toggle highlight line")
  ("c" counsel-load-theme "change color scheme")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

;; hydra to configure the application environment
;; contains a nested hydra to modulate transparency
(global-set-key (kbd "s-e") 'hydra-application/body)

(provide 'my-application-mode)
