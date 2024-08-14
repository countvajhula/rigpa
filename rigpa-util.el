;;; rigpa-util.el --- Start writing, stay focused, don't worry -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/rigpa

;; This work is "part of the world." You are free to do whatever you
;; like with it and it isn't owned by anybody, not even the creators.
;; Attribution would be appreciated and would help, but it is not
;; strictly necessary nor required.

;; The freely released, copyright-free work in this repository
;; represents an investment in a better way of doing things called
;; attribution-based economics (ABE). Attribution-based economics is
;; based on the simple idea that we gain more by giving more, not by
;; holding on to things that, truly, we could only create because we,
;; in our turn, received from others. As it turns out, an economic
;; system based on attribution -- where those who give more are more
;; empowered -- is significantly more efficient than capitalism while
;; also being stable and fair (unlike capitalism, on both counts),
;; giving it transformative power to elevate the human condition and
;; address the problems that face us today along with a host of others
;; that have been intractable since the beginning. You can help make
;; this a reality by releasing your work in the same way -- freely
;; into the public domain in the simple hope of providing value. Learn
;; more about attribution-based economics at drym.org, tell your
;; friends, do your part.

;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.
;;

;;; Commentary:

;; General utilities for Rigpa

;;; Code:

(defun rigpa--for-all-buffers (action)
  "Take ACTION for all open buffers.

ACTION must take no arguments and should return nothing.  If a return
value is desired, then use a closure with a mutable lexical variable,
and mutate that variable in ACTION."
  (let ((blist (buffer-list)))
    (while blist
      (with-current-buffer (car blist)
        (funcall action))
      (setq blist (cdr blist)))))

(provide 'rigpa-util)
;;; rigpa-util.el ends here
