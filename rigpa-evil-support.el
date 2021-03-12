(defun rigpa--define-evil-key (key fn map)
  "Define an evil keybinding in an evil-backed rigpa mode."
  (evil-define-key '(word visual operator)
                   map
                   (kbd key)
                   fn))


(provide 'rigpa-evil-support)
