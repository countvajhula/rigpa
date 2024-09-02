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
(require 'lithium)
(require 'chimera)
(require 'centaur-tabs)
(require 'beacon)

(defvar rigpa-application--original-transparency 100)

(evil-define-state application
  "Application state."
  :tag " <A> "
  :message "-- APPLICATION --")

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

(fset (intern "rigpa-application-load-theme")
      (symbol-function
       (cond ((require 'counsel nil t)
              'counsel-load-theme)
             ((require 'consult nil t)
              'consult-theme)
             (t 'load-theme))))

(lithium-define-global-mode rigpa-application-mode
  "Application mode"
  (("y" hydra-transparency/body t)
   ("t" centaur-tabs-mode t)
   ("n" display-line-numbers-mode t)
   ("b" rigpa-application-toggle-alarm-bell t)
   ("B" beacon-mode t)
   ("s" scroll-bar-mode t)
   ("l" hl-line-mode t)
   ("c" rigpa-application-load-theme t)
   ("f" set-frame-font t)
   ("<return>" rigpa-enter-lower-level)
   ("<escape>" rigpa-enter-higher-level))
  :lighter " application"
  :group 'rigpa)

(defun rigpa--on-application-mode-entry ()
  "Actions to take upon entering application mode."
  (evil-application-state))

(defun rigpa--on-application-mode-post-exit ()
  "Actions to take upon exiting application mode."
  (rigpa--enter-appropriate-mode))

(defvar chimera-application-mode
  (make-chimera-mode :name "application"
                     :enter #'rigpa-application-mode-enter
                     :exit #'rigpa-application-mode-exit
                     :pre-entry-hook 'rigpa-application-mode-pre-entry-hook
                     :post-exit-hook 'rigpa-application-mode-post-exit-hook
                     :entry-hook 'rigpa-application-mode-post-entry-hook
                     :exit-hook 'rigpa-application-mode-pre-exit-hook
                     :manage-hooks nil))


(provide 'rigpa-application-mode)
;;; rigpa-application-mode.el ends here
