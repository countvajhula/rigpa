;;; rigpa-meta.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; Utilities to support meta modes
;;

;;; Code:

(require 'rigpa-types)

(defun rigpa--get-ground-buffer ()
  "Get the ground buffer.

At the lowest level, the buffer is expected to refer to itself
to terminate the reference chain."
  (unless rigpa--ground-buffer
    (setq rigpa--ground-buffer (current-buffer)))
  rigpa--ground-buffer)

(defun rigpa--buffer-name (ensemble)
  "Buffer name to use for a given ensemble."
  (concat rigpa-buffer-prefix "-" (rigpa-editing-entity-name ensemble)))

(defun rigpa--set-meta-buffer-appearance ()
  "Configure meta mode buffer appearance."
  (buffer-face-set 'rigpa-face)
  (text-scale-set 5)
  ;;(setq cursor-type nil))
  (internal-show-cursor nil nil)
  (when display-line-numbers-mode
    (display-line-numbers-mode -1)))

(defun rigpa--set-ui-for-meta-modes ()
  "Set (for now, global) UI parameters for meta modes."
  ;; should ideally be perspective-specific
  (blink-cursor-mode -1))

(defun rigpa--revert-ui ()
  "Revert buffer appearance to settings prior to entering mode mode."
  (blink-cursor-mode 1))


(provide 'rigpa-meta)
;;; rigpa-meta.el ends here
