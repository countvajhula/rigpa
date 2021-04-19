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

(require 'chimera)

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

(defun rigpa-ensemble-member-position-by-name (ensemble name)
  "The position of a member in an ensemble, by name."
  (seq-position (seq-map #'rigpa-editing-entity-name
                         (editing-ensemble-members ensemble))
                name))

(defun rigpa-ensemble-size (ensemble)
  "Size of ensemble (e.g. height of a tower)."
  (length (editing-ensemble-members ensemble)))

(defun rigpa-ensemble-member-at-position (tower position)
  "Mode at LEVEL in the TOWER."
  (nth position (editing-ensemble-members tower)))

(defun rigpa--member-of-ensemble-p (entity ensemble)
  "A predicate asserting whether ENTITY is a member of ENSEMBLE."
  (memq entity (editing-ensemble-members ensemble)))


(provide 'rigpa-types)
;;; rigpa-types.el ends here
