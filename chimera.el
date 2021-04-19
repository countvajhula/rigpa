;;; chimera.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; Chimera modal interface abstraction layer
;;

;;; Code:

(cl-defstruct chimera-mode
  "Specification for a mode."
  name
  (enter nil :documentation "Primitive mode entry function.") ; this is required
  (exit nil :documentation "Primitive mode exit function.") ; we don't need to rely on exit being defined
  (pre-entry-hook nil)
  (entry-hook nil)
  (exit-hook nil)
  (post-exit-hook nil)
  (manage-hooks nil
                :documentation "Whether within-mode hooks should be managed internally. \
If not, they are expected to be run by the underlying mode provider \
(e.g. evil or hydra). Wrapping hooks (pre-entry and post-exit) are \
always managed by chimera."))

(defvar chimera-evil-states
  (list "normal"
        "insert"
        "emacs"
        "visual"
        "replace"
        "operator"
        "word"
        "line"
        "char"
        "symex"))

(defvar chimera-insertion-states
  (list "insert" "emacs"))

(defun chimera-enter-mode (mode)
  "Enter MODE."
  (interactive)
  (let ((name (chimera-mode-name mode)))
    ;; TODO: maybe call a function (perform-entry-actions ...) that
    ;; handles any provider-specific jankiness, like checking for
    ;; hydras that didn't exit cleanly, and perform their exit actions
    ;; (which should be in a dedicated function that can be called
    ;; from here as well as the original spot in the hydra exit
    ;; lifecycle phase).
    ;; we're using evil state variables to keep track of state (even
    ;; for non-evil backed modes), so ensure that the evil state is
    ;; entered here
    (unless (member name chimera-evil-states)
      (let ((evil-state-entry (intern (concat "evil-" name "-state"))))
        (funcall evil-state-entry)))
    (run-hooks (chimera-mode-pre-entry-hook mode))
    (funcall (chimera-mode-enter mode))
    (when (chimera-mode-manage-hooks mode)
      ;; for now, we rely on evil hooks for all modes (incl.
      ;; hydra-based ones), and this should never be called.
      (run-hooks (chimera-mode-entry-hook mode)))))

(defun chimera-exit-mode (mode)
  "Exit (interrupt) MODE."
  (interactive)
  (funcall (chimera-mode-exit mode)))

(defun chimera--mode-for-state (mode-name)
  (symbol-value (intern (concat "chimera-" mode-name "-mode"))))


(provide 'chimera)
;;; chimera.el ends here
