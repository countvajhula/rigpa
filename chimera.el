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

(require 'cl-lib)
(require 'evil)

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

;; TODO: note name confusion
(defun rigpa-current-mode ()
  "Current rigpa mode."
  (chimera--mode-for-state (symbol-name evil-state)))

(defun chimera-enter-mode (mode)
  "Enter MODE."
  (interactive)
  (let ((current-mode (rigpa-current-mode))
        (name (chimera-mode-name mode)))
    (chimera--exit-mode current-mode)
    ;; This is used for enabling evil-specific minor modes
    ;; that need to be enabled _before_ mode entry.
    ;; See docs for `rigpa--minor-mode-enabler'
    ;; Would be great if we could avoid the need for this.
    (run-hooks (chimera-mode-pre-entry-hook mode))
    (chimera--enter-mode mode)
    ;; we're using evil state variables to keep track of state (even
    ;; for non-evil backed modes), so ensure that the evil state is
    ;; entered here
    (unless (member name chimera-evil-states)
      (let ((evil-state-entry (intern (concat "evil-" name "-state"))))
        (funcall evil-state-entry)))))

(defun chimera--enter-mode (mode)
  "Enter MODE."
  (let ((enter-mode (chimera-mode-enter mode)))
    (funcall enter-mode)))

(defun chimera--exit-mode (mode)
  "Exit (interrupt) MODE."
  (let ((exit-mode (chimera-mode-exit mode)))
    (when exit-mode
      (funcall exit-mode))))

(defun chimera--mode-for-state (mode-name)
  (symbol-value (intern (concat "chimera-" mode-name "-mode"))))


(provide 'chimera)
;;; chimera.el ends here
