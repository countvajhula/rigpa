;;; rigpa-history-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to the file's history of changes
;;

;;; Code:

(require 'evil)
(require 'hydra)
(require 'chimera)
(require 'chimera-hydra)
(require 'git-timemachine)

(evil-define-state history
  "History state."
  :tag " <C> "
  :message "-- x→o --"
  :enable (normal))

(defhydra hydra-history (:columns 2
                         ; maybe put body-pre in ad hoc entry
                         :body-pre (progn (unless git-timemachine-mode (git-timemachine))
                                          (chimera-hydra-signal-entry chimera-history-mode))
                         :post (chimera-hydra-portend-exit chimera-history-mode t)
                         :after-exit (chimera-hydra-signal-exit chimera-history-mode
                                                                #'chimera-handle-hydra-exit))
  "History mode"
  ("h" git-timemachine-show-previous-revision "previous")
  ("l" git-timemachine-show-next-revision "next")
  ("M-l" git-timemachine-show-current-revision "latest")
  ("b" git-timemachine-blame "annotate history ('blame')" :exit t)
  ("/" git-timemachine-show-revision-fuzzy "search")
  ("?" git-timemachine-show-commit "help (show commit)")
  ("q" git-timemachine-quit "return to the present" :exit t)
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-history-mode-entry-hook nil
  "Entry hook for rigpa history mode.")

(defvar chimera-history-mode-exit-hook nil
  "Exit hook for rigpa history mode.")

(defvar chimera-history-mode
  (make-chimera-mode :name "history"
                     :enter #'hydra-history/body
                     :pre-entry-hook 'chimera-history-mode-entry-hook
                     :post-exit-hook 'chimera-history-mode-exit-hook
                     :entry-hook 'evil-history-state-entry-hook
                     :exit-hook 'evil-history-state-exit-hook))


(provide 'rigpa-history-mode)
;;; rigpa-history-mode.el ends here
