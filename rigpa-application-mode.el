;;; rigpa-application-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to the application (i.e. Emacs)
;;

;;; Code:

(require 'evil)
(require 'hydra)
(require 'chimera-hydra)
(require 'centaur-tabs)
(require 'beacon)
(require 'counsel)

(defvar rigpa-application--original-transparency 100)

(evil-define-state application
  "Application state."
  :tag " <A> "
  :message "-- APPLICATION --"
  :enable (normal))

(defun rigpa-application-toggle-alarm-bell ()
  "Toggle whether the alarm bell sounds."
  (interactive)
  (setq ring-bell-function (if ring-bell-function
                               nil
                             'ignore)))

(defun rigpa-application-current-transparency ()
  (or (nth 0
           (frame-parameter (selected-frame)
                            'alpha))
      100))

(defun rigpa-application--bound (value min-bound max-bound)
  "Bound a value within the provided range."
  (min (max value min-bound) max-bound))

;; Set transparency of emacs
;; From: https://www.emacswiki.org/emacs/TransparentEmacs
(defun rigpa-application-set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha (cons value value)))

(defun rigpa-application--adjust-transparency (delta)
  "Adjust the transparency of the frame window by the configured delta,
   in the range: 0=transparent/100=opaque"
  (interactive)
  (rigpa-application-set-transparency
   (rigpa-application--bound (+ (rigpa-application-current-transparency)
                                delta)
                             0
                             100)))

(defun rigpa-application-increase-transparency (&optional superlative)
  "Increase frame transparency."
  (interactive)
  (cond ((eq superlative nil)
         (rigpa-application--adjust-transparency -1))
        ((eq superlative 'more)
         (rigpa-application--adjust-transparency -5))
        (t (rigpa-application-maximize-transparency))))

(defun rigpa-application-decrease-transparency (&optional superlative)
  "Decrease frame transparency."
  (interactive)
  (cond ((eq superlative nil)
         (rigpa-application--adjust-transparency 1))
        ((eq superlative 'more)
         (rigpa-application--adjust-transparency 5))
        (t (rigpa-application-minimize-transparency))))

(defun rigpa-application-maximize-transparency ()
  "Maximize frame transparency (i.e. make transparent)"
  (interactive)
  (rigpa-application-set-transparency 0))

(defun rigpa-application-minimize-transparency ()
  "Minimize frame transparency (i.e. make opaque)"
  (interactive)
  (rigpa-application-set-transparency 100))

(defun rigpa-application-return-to-original-transparency ()
  "Return to original transparency prior to making changes."
  (interactive)
  (rigpa-application-set-transparency rigpa-application--original-transparency))

(defhydra hydra-transparency (:columns 1
                              :body-pre (setq rigpa-application--original-transparency
                                              (rigpa-application-current-transparency)))
  "Control frame transparency"
  ("+" rigpa-application-decrease-transparency "decrease transparency")
  ("-" rigpa-application-increase-transparency "increase transparency")
  ("k" rigpa-application-decrease-transparency "decrease transparency")
  ("C-k" (lambda ()
           (interactive)
           (rigpa-application-decrease-transparency 'more)) "decrease transparency more")
  ("j" rigpa-application-increase-transparency "increase transparency")
  ("C-j" (lambda ()
           (interactive)
           (rigpa-application-increase-transparency 'more)) "increase transparency more")
  ("M-k" rigpa-application-minimize-transparency "least transparent (opaque)")
  ("M-j" rigpa-application-maximize-transparency "most transparent")
  ("q" rigpa-application-return-to-original-transparency  "return to original transparency" :exit t)
  ("<escape>" ignore "quit" :exit t))

(defhydra hydra-application (:columns 1
                             :exit t
                             :body-pre (chimera-hydra-signal-entry chimera-application-mode)
                             :post (chimera-hydra-portend-exit chimera-application-mode t)
                             :after-exit (chimera-hydra-signal-exit chimera-application-mode
                                                                    #'chimera-handle-hydra-exit))
  "Control application environment"
  ("y" hydra-transparency/body "transparency")
  ("t" centaur-tabs-mode "toggle tabs")
  ("n" display-line-numbers-mode "toggle line numbers")
  ("b" rigpa-application-toggle-alarm-bell "toggle alarm bell")
  ("B" beacon-mode "toggle beacon")
  ("s" scroll-bar-mode "toggle scroll bar")
  ("l" hl-line-mode "toggle highlight line")
  ("c" counsel-load-theme "change color scheme")
  ("H-m" rigpa-toggle-menu "show/hide this menu" :exit nil)
  ("<return>" rigpa-enter-lower-level "enter lower level")
  ("<escape>" rigpa-enter-higher-level "escape to higher level"))

(defvar chimera-application-mode-entry-hook nil
  "Entry hook for rigpa application mode.")

(defvar chimera-application-mode-exit-hook nil
  "Exit hook for rigpa application mode.")

(defvar chimera-application-mode
  (make-chimera-mode :name "application"
                     :enter #'hydra-application/body
                     :pre-entry-hook 'chimera-application-mode-entry-hook
                     :post-exit-hook 'chimera-application-mode-exit-hook
                     :entry-hook 'evil-application-state-entry-hook
                     :exit-hook 'evil-application-state-exit-hook))


(provide 'rigpa-application-mode)
;;; rigpa-application-mode.el ends here
