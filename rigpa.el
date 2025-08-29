;;; rigpa.el --- Self-reflective editing modes -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/rigpa
;; Version: 0.5
;; Package-Requires: ((emacs "26.1") (evil "1.2.14") (hydra "0.15.0") (lithium "0.1") (dynaring "0.3") (buffer-ring "0.3.4") (ivy "0.13.0") (centaur-tabs "3.1") (beacon "1.3.4") (dictionary "1.11") (ace-window "0.9.0") (git-timemachine "4.11") (parsec "0.1.3") (ht "2.0") (s "1.12.0") (dash "2.18.0") (transpose-frame "0.2.0") (auto-dim-other-buffers "20220209.2101"))
;; Keywords: emulations, frames, convenience

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

;;; Commentary:
;;
;; Rigpa allows you to construct "towers" of editing modes
;; (in principle, these could be any evil modes) and manipulate which
;; tower is active at any point in time.  It generalizes both vim's
;; notion of editing mode, as well as Emacs's notion of a major mode
;; into a unified way of looking at things, that is, as towers of
;; modes which can be swapped and themselves edited using the very
;; modes they contain.
;;
;; In addition, rigpa also defines conventions that modes
;; should follow in order to be seamlessly integrated
;; into editing towers.  This includes conventions around keybindings
;; for moving up and down the hierarchy of editing levels, standard
;; semantics of modifier keys, defining a canonical action for each
;; mode, and other such conventions to ensure semantic uniformity across
;; editing levels.

;;; Code:

(require 'evil)
(require 'chimera)
(require 'ht)
(require 'rigpa-custom)
(require 'rigpa-mode-mode)
(require 'rigpa-char-mode)
(require 'rigpa-word-mode)
(require 'rigpa-line-mode)
(require 'rigpa-view-mode)
(require 'rigpa-window-mode)
(require 'rigpa-file-mode)
(require 'rigpa-buffer-mode)
(require 'rigpa-system-mode)
(require 'rigpa-application-mode)
(require 'rigpa-activity-mode)
(require 'rigpa-text-mode)
(require 'rigpa-tab-mode)
(require 'rigpa-history-mode)
(require 'rigpa-tower-mode)

;; define face for use in meta modes
(make-face 'rigpa-face)
(set-face-font 'rigpa-face
               (or (find-font (font-spec :name "Consolas"))
                   (find-font (font-spec :name "Courier New"))
                   (face-attribute 'default :font)))

(set-face-foreground 'rigpa-face "tomato")

;; the prefix that will be used in naming all buffers used
;; in meta mode representations
(defvar rigpa-buffer-prefix "*RIGPA-META*")

(define-derived-mode rigpa-meta-mode
  text-mode "Meta"
  "Major mode for meta modes"
  (define-key rigpa-meta-mode-map (kbd "g r") 'rigpa--reload-tower))

(define-derived-mode rigpa-meta-tower-mode
  text-mode "Tower"
  "Major mode for meta modes"
  (define-key rigpa-meta-tower-mode-map (kbd "g r") 'rigpa--reload-tower))

;; wrap native evil states in chimera modes
(defvar chimera-normal-mode-entry-hook nil
  "Entry hook for rigpa normal mode.")

(defvar chimera-normal-mode-exit-hook nil
  "Exit hook for rigpa normal mode.")

(defvar chimera-normal-mode
  (make-chimera-mode :name "normal"
                     :enter #'evil-normal-state
                     :pre-entry-hook 'chimera-normal-mode-entry-hook
                     :post-exit-hook 'chimera-normal-mode-exit-hook
                     :entry-hook 'evil-normal-state-entry-hook
                     :exit-hook 'evil-normal-state-exit-hook))

(defvar chimera-insert-mode-entry-hook nil
  "Entry hook for rigpa insert mode.")

(defvar chimera-insert-mode-exit-hook nil
  "Exit hook for rigpa insert mode.")

(defvar chimera-insert-mode
  (make-chimera-mode :name "insert"
                     :enter #'evil-insert-state
                     :pre-entry-hook 'chimera-insert-mode-entry-hook
                     :post-exit-hook 'chimera-insert-mode-exit-hook
                     :entry-hook 'evil-insert-state-entry-hook
                     :exit-hook 'evil-insert-state-exit-hook))

(defvar chimera-emacs-mode-entry-hook nil
  "Entry hook for rigpa emacs mode.")

(defvar chimera-emacs-mode-exit-hook nil
  "Exit hook for rigpa emacs mode.")

(defvar chimera-emacs-mode
  (make-chimera-mode :name "emacs"
                     :enter #'evil-emacs-state
                     :pre-entry-hook 'chimera-emacs-mode-entry-hook
                     :post-exit-hook 'chimera-emacs-mode-exit-hook
                     :entry-hook 'evil-emacs-state-entry-hook
                     :exit-hook 'evil-emacs-state-exit-hook))

(defvar chimera-visual-mode-entry-hook nil
  "Entry hook for rigpa visual mode.")

(defvar chimera-visual-mode-exit-hook nil
  "Exit hook for rigpa visual mode.")

(defvar chimera-visual-mode
  (make-chimera-mode :name "visual"
                     :enter #'evil-visual-state
                     :pre-entry-hook 'chimera-visual-mode-entry-hook
                     :post-exit-hook 'chimera-visual-mode-exit-hook
                     :entry-hook 'evil-visual-state-entry-hook
                     :exit-hook 'evil-visual-state-exit-hook))

(defvar chimera-replace-mode-entry-hook nil
  "Entry hook for rigpa replace mode.")

(defvar chimera-replace-mode-exit-hook nil
  "Exit hook for rigpa replace mode.")

(defvar chimera-replace-mode
  (make-chimera-mode :name "replace"
                     :enter #'evil-replace-state
                     :pre-entry-hook 'chimera-replace-mode-entry-hook
                     :post-exit-hook 'chimera-replace-mode-exit-hook
                     :entry-hook 'evil-replace-state-entry-hook
                     :exit-hook 'evil-replace-state-exit-hook))

(defvar chimera-operator-mode-entry-hook nil
  "Entry hook for rigpa operator mode.")

(defvar chimera-operator-mode-exit-hook nil
  "Exit hook for rigpa operator mode.")

(defvar chimera-operator-mode
  (make-chimera-mode :name "operator"
                     :enter #'evil-operator-state
                     :pre-entry-hook 'chimera-operator-mode-entry-hook
                     :post-exit-hook 'chimera-operator-mode-exit-hook
                     :entry-hook 'evil-operator-state-entry-hook
                     :exit-hook 'evil-operator-state-exit-hook))

(defun rigpa--enter-lower-or-pass-through ()
  "Enter a lower level, or pass through to underlying keymap."
  (interactive)
  (if buffer-read-only
      ;; ideally should find a way to "pass through"
      ;; to lower-priority keymaps instead of specifying
      ;; what should happen on a per-mode basis here
      (cond ((eq major-mode 'dired-mode)
             (dired-find-file))
            ((memq major-mode '(help-mode
                                debugger-mode
                                ert-results-mode
                                racket-describe-mode))
             (push-button))
            ((eq major-mode 'occur-mode)
             (occur-mode-goto-occurrence))
            ((eq major-mode 'grep-mode)
             (compile-goto-error))
            ((eq major-mode 'ivy-occur-grep-mode)
             (ivy-occur-press-and-switch))
            ((eq major-mode 'dictionary-mode)
             (link-selected))
            ((eq major-mode 'ripgrep-search-mode)
             (compile-goto-error))
            ((eq major-mode 'magit-status-mode)
             (magit-diff-visit-file (magit-current-file) t))
            ((eq major-mode 'magit-log-mode)
             (magit-show-commit (magit-thingatpt--git-revision)))
            ((eq major-mode 'Info-mode)
             (Info-follow-nearest-node))
            ((eq major-mode 'xref--xref-buffer-mode)
             (xref-goto-xref))
            ((eq major-mode 'ibuffer-mode)
             (ibuffer-visit-buffer))
            ((eq major-mode 'rst-toc-mode)
             (rst-toc-mode-follow-link-kill))
            ((eq major-mode 'sr-mode)
             (sr-advertised-find-file))
            (t (rigpa-enter-lower-level)))
    (cond ((eq major-mode 'Custom-mode)
           (Custom-newline (point)))
          (t (rigpa-enter-lower-level)))))

(defun rigpa--integrate-evil-states ()
  "Map standard evil state entry and exit points so they're managed by rigpa."
  ;; evil interop keybindings
  ;; TODO: these (Esc/Ret) should be dependent on whether there are any
  ;; other modes in the tower. If not, then this shouldn't be bound
  ;; IOW this keybinding (and some class of bindings more generally)
  ;; is tower-specific
  (dolist (state chimera-evil-states)
    (let ((keymap (symbol-value
                   (intern
                    (concat "evil-" state "-state-map")))))
      (define-key keymap [escape] #'rigpa-enter-higher-level)
      (unless (member state chimera-insertion-states)
        (define-key keymap [return] #'rigpa--enter-lower-or-pass-through))))
  ;; exit visual state gracefully
  (define-key evil-visual-state-map [escape] (lambda ()
                                               (interactive)
                                               (evil-exit-visual-state)
                                               (rigpa-enter-higher-level)))
  (define-key evil-visual-state-map [return] (lambda ()
                                               (interactive)
                                               (evil-exit-visual-state)
                                               (rigpa-enter-lower-level)))
  ;; interrupting operator state should unconditionally "escape"
  ;; but by default an operator enters insert state as a follow-on.
  ;; we use the default normal override binding here to avoid this.
  ;; This will bypass any rigpa-specific behavior, but as it seems
  ;; unlikely that we'd want to incorporate operator state formally
  ;; as part of any structures, this seems a reasonable hack
  (define-key evil-operator-state-map [escape] #'evil-force-normal-state)
  ;; same, I guess, for replace state? though, why are we even overriding
  ;; [esc] above to begin with? Should we not integrate built-in evil
  ;; states other than Normal?
  (define-key evil-replace-state-map [escape] #'evil-force-normal-state))

(defun rigpa--register-local-mode (mode)
  "Register the local mode MODE."
  (rigpa-register-mode mode
                       :post-entry (rigpa-evil-state-by-name
                                    (chimera-mode-name mode))
                       :post-exit #'rigpa--enter-local-evil-state))

(defun rigpa--register-global-mode (mode)
  "Register the global mode MODE."
  (rigpa-register-mode mode
                       :post-entry (lambda ()
                                     (rigpa--for-all-buffers (rigpa-evil-state-by-name
                                                              (chimera-mode-name mode))))
                       :post-exit (lambda ()
                                    (rigpa--for-all-buffers #'rigpa--enter-local-evil-state))))

(defun rigpa--register-modes ()
  "Register the built-in modes with the framework.

This adds them to a global mode registry, `rigpa-modes', so that they
can be looked up by name, and also registers functions to listen on
mode transitions to ensure that the correct mode is reflected and to
keep track of the history of mode transitions for \"recall\" purposes.
It also adds internal hooks relevant for Rigpa (currently, that's just
entering and exiting the associated Evil state for the mode, which is
done purely for UI purposes)."

  ;; register evil chimera states with the rigpa framework
  (rigpa-register-mode chimera-normal-mode)
  (rigpa-register-mode chimera-insert-mode)
  (rigpa-register-mode chimera-emacs-mode)
  (rigpa-register-mode chimera-visual-mode)
  (rigpa-register-mode chimera-replace-mode)
  (rigpa-register-mode chimera-operator-mode)

  ;; register all the other modes

  ;; TODO: Line mode is _nonlocal_ to the tower, and yet
  ;; buffer-local. So upon exit, we ideally want to return
  ;; to a tower-local mode here, instead of leaving it
  ;; hanging, or, as we are doing here, entering an _evil_
  ;; state explicitly (which would happen as a side effect
  ;; of the right thing, viz. returning to the tower).
  (rigpa--register-local-mode chimera-line-mode)
  (rigpa--register-local-mode chimera-application-mode)
  (rigpa--register-local-mode chimera-view-mode)
  (rigpa--register-local-mode chimera-activity-mode)
  (rigpa--register-local-mode chimera-tab-mode)
  (rigpa--register-local-mode chimera-word-mode)
  (rigpa--register-local-mode chimera-char-mode)
  (rigpa--register-local-mode chimera-system-mode)
  (rigpa--register-local-mode chimera-file-mode)
  (rigpa--register-local-mode chimera-text-mode)
  ;; TODO: for buffer mode, probably enter appropriate mode in current
  ;; and original buffer
  ;; we can enter appropriate in original if different from current buffer
  (rigpa--register-global-mode chimera-buffer-mode)
  (rigpa--register-global-mode chimera-history-mode)
  (rigpa--register-global-mode chimera-window-mode))

(defun rigpa--initialize-modes ()
  "Initialize all built-in modes."
  (rigpa-line-initialize)
  (rigpa-activity-initialize)
  (rigpa-view-initialize)
  (rigpa-application-initialize)
  (rigpa-history-initialize)
  (rigpa-tab-initialize)
  (rigpa-word-initialize)
  (rigpa-window-initialize)
  (rigpa-char-initialize)
  (rigpa-system-initialize)
  (rigpa-buffer-initialize)
  (rigpa-file-initialize)
  (rigpa-text-initialize))

;; towers in base editing levels
(defvar rigpa-complete-tower
  (make-editing-ensemble :name "complete"
                         :default "normal"
                         :members (list chimera-insert-mode
                                        chimera-char-mode
                                        chimera-word-mode
                                        chimera-line-mode
                                        chimera-activity-mode
                                        chimera-normal-mode
                                        chimera-view-mode
                                        chimera-window-mode
                                        chimera-file-mode
                                        chimera-buffer-mode
                                        chimera-system-mode
                                        chimera-application-mode)))

(defvar rigpa-vim-tower
  (make-editing-ensemble :name "vim"
                         :default "normal"
                         :members (list chimera-insert-mode
                                        chimera-normal-mode)))
(defvar rigpa-emacs-tower
  (make-editing-ensemble :name "emacs"
                         :default "emacs"
                         :members (list chimera-emacs-mode)))

;; complexes for base editing levels
(defvar rigpa-general-complex
  (make-editing-ensemble
   :name "general"
   :default "vim"
   :members (list rigpa-vim-tower
                  rigpa-complete-tower
                  rigpa-emacs-tower))
  "A default editing complex for general use.")

;; towers and complexes for meta levels
(defvar rigpa-meta-mode-tower
  (make-editing-ensemble :name "meta-mode"
                         :default "line"
                         :members (list chimera-line-mode)))
(defvar rigpa-meta-complex
  (make-editing-ensemble
   :name "meta"
   :default "meta-mode"
   :members (list rigpa-meta-mode-tower)))

(defvar rigpa-meta-tower-mode-tower
  (make-editing-ensemble :name "meta-tower"
                         :default "buffer"
                         :members (list chimera-buffer-mode)))

(defvar rigpa-meta-tower-complex
  (make-editing-ensemble
   :name "meta-tower"
   :default "meta-tower"
   :members (list rigpa-meta-tower-mode-tower)))

(defvar rigpa--complex rigpa-general-complex
  "The current editing complex.

By default this is general complex unless a more tailored one
has been set in a buffer (e.g. via major mode hook).

It is defined here as an ordinary global variable, but it is made
buffer local below so that it can have a default value that can be
overridden in individual buffers.")

(make-variable-buffer-local 'rigpa--complex)

(defun rigpa--provide-editing-structures ()
  "Register editing structures so they're used in relevant major modes."
  ;; change these to use a state struct instead of globals
  ;; that state struct must specify
  ;;  (1) the complex, as an object (this should take care of default tower
  ;;      so there's no need to set the tower index initially)
  ;;  (2) the tower index
  ;;  (3) the level index
  ;; and eventually make these "coordinates" generic
  (add-hook 'rigpa-meta-mode-hook
            ;; TODO: dispatch here based on meta level. If the level
            ;; is 1 use line mode / buffers, if it's 2,
            ;; use buffer mode / perspectives?
            (lambda ()
              (setq rigpa--complex rigpa-meta-complex)))
  (add-hook 'rigpa-meta-tower-mode-hook
            ;; TODO: dispatch here based on meta level. If the level
            ;; is 1 use line mode / buffers, if it's 2,
            ;; use buffer mode / perspectives?
            (lambda ()
              (setq rigpa--complex rigpa-meta-tower-complex))))

(defun rigpa-initialize-evil ()
  "Initialize some evil interop."
  (advice-add 'evil-repeat
              :around #'rigpa-evil-preserve-state-advice)
  ;; undo resets to normal state if there are no
  ;; changes to undo (for some reason. it doesn't do
  ;; this if an action is there to undo)
  (advice-add 'evil-undo
              :around #'rigpa-evil-preserve-state-advice))

(defun rigpa-initialize ()
  "Initialize rigpa."
  (interactive)
  (unless lithium-mode
    (lithium-mode 1))
  (rigpa--initialize-modes)
  (rigpa--register-modes)
  (rigpa--provide-editing-structures)
  ;; should make this optional via a defcustom flag
  ;; or potentially even have it in a separate evil-adapter package
  (when (boundp 'evil-mode)
    (rigpa--integrate-evil-states)
    (rigpa-initialize-evil)))

(defun rigpa-disable ()
  "Disable rigpa."
  (interactive)
  ;; TODO:
  ;; remap evil keybindings
  ;; unregister major mode hooks
  ;; unregister all modes (including esp evil states)
  ;; remove evil advice
  nil)

;;;###autoload
(define-minor-mode rigpa-mode
  "A modal UI framework."
  :lighter " rigpa"
  :global t
  :group 'rigpa
  (if rigpa-mode
      (rigpa-initialize)
    (rigpa-disable)))


(provide 'rigpa)
;;; rigpa.el ends here
