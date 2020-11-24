(require 'chimera-hydra)

(evil-define-state application
  "Application state."
  :tag " <A> "
  :message "-- APPLICATION --"
  :enable (normal))

(defun my-toggle-alarm-bell ()
  "Toggle whether the alarm bell sounds."
  (interactive)
  (setq ring-bell-function (if ring-bell-function
                               nil
                             'ignore)))

(defun current-transparency ()
  (nth 0
       (frame-parameter (selected-frame)
			'alpha)))

(defun bound (value min-bound max-bound)
  "Bound a value within the provided range."
  (min (max value min-bound) max-bound))

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
  (transparency (bound (+ (current-transparency)
                          delta)
                       0
                       100)))

(defun increase-transparency (&optional superlative)
  "Increase frame transparency."
  (interactive)
  (cond ((eq superlative nil)
         (adjust-transparency -1))
        ((eq superlative 'more)
         (adjust-transparency -5))
        (t (maximize-transparency))))

(defun decrease-transparency (&optional superlative)
  "Decrease frame transparency."
  (interactive)
  (cond ((eq superlative nil)
         (adjust-transparency 1))
        ((eq superlative 'more)
         (adjust-transparency 5))
        (t (minimize-transparency))))

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
  ("C-k" (lambda ()
           (interactive)
           (decrease-transparency 'more)) "decrease transparency more")
  ("j" increase-transparency "increase transparency")
  ("C-j" (lambda ()
           (interactive)
           (increase-transparency 'more)) "increase transparency more")
  ("M-k" minimize-transparency "least transparent (opaque)")
  ("M-j" maximize-transparency "most transparent")
  ("q" return-to-original-transparency  "return to original transparency" :exit t)
  ("<escape>" my-noop "quit" :exit t))

(defhydra hydra-application (:columns 1
                             :exit t
                             :post (chimera-hydra-portend-exit chimera-application-mode t)
                             :after-exit (chimera-hydra-signal-exit chimera-application-mode
                                                                    #'chimera-handle-hydra-exit))
  "Control application environment"
  ("t" hydra-transparency/body "transparency")
  ("n" display-line-numbers-mode "toggle line numbers")
  ("b" my-toggle-alarm-bell "toggle alarm bell")
  ("l" hl-line-mode "toggle highlight line")
  ("c" counsel-load-theme "change color scheme")
  ("H-m" eem-toggle-menu "show/hide this menu" :exit nil)
  ("<return>" eem-enter-lower-level "enter lower level")
  ("<escape>" eem-enter-higher-level "escape to higher level"))

(defvar chimera-application-mode-entry-hook nil
  "Entry hook for epistemic application mode.")

(defvar chimera-application-mode-exit-hook nil
  "Exit hook for epistemic application mode.")

(defvar chimera-application-mode
  (make-chimera-mode :name "application"
                     :enter #'hydra-application/body
                     :entry-hook 'evil-application-state-entry-hook
                     :exit-hook 'evil-application-state-exit-hook))


(provide 'eem-application-mode)
