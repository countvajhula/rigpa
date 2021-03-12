(defun rigpa--define-evil-key (key fn map)
  "Define an evil keybinding in an evil-backed rigpa mode."
  (evil-define-key '(word visual operator)
                   map
                   (kbd key)
                   fn))

(defun rigpa--define-evil-keys-from-spec (keyspec keymap)
  "Define evil keys from a specification."
  (dolist (keybinding keyspec)
    (rigpa--define-evil-key (car keybinding)
                            (cdr keybinding)
                            keymap)))


(provide 'rigpa-evil-support)
