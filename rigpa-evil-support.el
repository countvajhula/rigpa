(defun rigpa--define-evil-key (key fn map state)
  "Define an evil keybinding in an evil-backed rigpa mode."
  (evil-define-key (list state 'visual 'operator)
                   map
                   (kbd key)
                   fn))

(defun rigpa--define-evil-keys-from-spec (keyspec keymap state)
  "Define evil keys from a specification."
  (dolist (keybinding keyspec)
    (rigpa--define-evil-key (car keybinding)
                            (cdr keybinding)
                            keymap
                            state)))


(provide 'rigpa-evil-support)
