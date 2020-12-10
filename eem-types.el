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
  (seq-position (seq-map #'rigpa-editing-entity-name
                         (editing-ensemble-members ensemble))
                name))

(defun rigpa-ensemble-size (ensemble)
  "Size of ensemble (e.g. height of a tower)."
  (length (editing-ensemble-members ensemble)))

(defun rigpa-ensemble-member-at-position (tower position)
  "Mode at LEVEL in the TOWER."
  (nth position (editing-ensemble-members tower)))


(provide 'rigpa-types)
;;; rigpa-types.el ends here
