;;; rigpa-types.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; Types and interfaces
;;

;;; Code:

(require 'cl-lib)
(require 'chimera)
(require 'dynaring)

(cl-defstruct editing-ensemble
  "Specification for an editing ensemble."
  name
  ;; TODO: members should be structs implementing an "entity" interface
  (members nil :documentation "A list of members of the editing ensemble.")
  (default nil :documentation "The canonical member of the tower."))

(cl-defgeneric rigpa-editing-entity-name (entity)
  "A generic function to access the name of any editing
entity, such as modes, towers or complexes.")

(cl-defmethod rigpa-editing-entity-name ((entity chimera-mode))
  (chimera-mode-name entity))

(cl-defmethod rigpa-editing-entity-name ((entity editing-ensemble))
  (editing-ensemble-name entity))

(defun rigpa--position-in-mode-list (modelist name)
  "Position of NAME in MODELIST.

Having lifted each mode into a list if it isn't already a list of
modes (the latter derived from a mode ring), we simply check if NAME
is a member of any of these lists of modes, returning the index of the
first mode list (corresponding to a level, typically) where that's
true.

This is just a quick hack in order to support levels being mode rings
and not just modes. One potentially better approach would be for
levels to *always* be mode rings, even if of size one."
  (when modelist
    (let ((ms (car modelist))
          (modelist (cdr modelist)))
      (if (member name ms)
          0
        (let ((result (rigpa--position-in-mode-list modelist
                                                    name)))
          (when result
            (1+ result)))))))

(defun rigpa-ensemble-member-position-by-name (ensemble name)
  "The position of a member in an ensemble, by name."
  (rigpa--position-in-mode-list (seq-map (lambda (m)
                                           (seq-map #'rigpa-editing-entity-name
                                                    (if (dynaringp m)
                                                        (dynaring-values m)
                                                      (list m))))
                                         (editing-ensemble-members ensemble))
                                name))

(defun rigpa--member-of-ensemble-p (ensemble entity-name)
  "A predicate asserting whether ENTITY-NAME is a member of ENSEMBLE."
  (not
   (not
    (rigpa-ensemble-member-position-by-name ensemble
                                            entity-name))))

(defun rigpa-ensemble-size (ensemble)
  "Size of ensemble (e.g. height of a tower)."
  (length (editing-ensemble-members ensemble)))

(defun rigpa-ensemble-member-at-position (tower position)
  "Mode at LEVEL in the TOWER."
  (nth position (editing-ensemble-members tower)))


(provide 'rigpa-types)
;;; rigpa-types.el ends here
