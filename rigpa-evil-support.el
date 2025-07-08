;;; rigpa-evil-support.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; Supporting utilities for evil-backed modes
;;

;;; Code:

(require 'evil)

(defun rigpa-evil-state-by-name (name)
  "Evil state handle for mode NAME, by string."
  (intern
   (concat "evil-"
           name
           "-state")))

(defun rigpa-evil-preserve-state-advice (orig-fun &rest args)
  "Return to an evil state if necessary after calling ORIG-FUN.

This function is meant to advise `evil-repeat' which sets the
buffer to normal state after repeating a command. This first
checks whether the buffer is starting in symex state and, if so,
returns to symex after invoking ORIG-FUN with ARGS."
  (let ((original-evil-state evil-state))
    (unwind-protect
        (apply orig-fun args)
      (when original-evil-state
        (funcall
         (intern
          (concat "evil-" (symbol-name original-evil-state) "-state")))))))

(evil-define-state activity
  "Activity state."
  :tag " <A> "
  :message "-- ACTIVITY --")

(evil-define-state line
  "Line state."
  :tag " <L> "
  :message "-- LINE --")

(evil-define-state view
  "View state."
  :tag " <V> "
  :message "-- VIEW --")

(evil-define-state application
  "Application state."
  :tag " <A> "
  :message "-- APPLICATION --")


(provide 'rigpa-evil-support)
;;; rigpa-evil-support.el ends here
