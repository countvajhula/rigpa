rigpa
=====

Modular editing levels and towers for Emacs.

Rigpa allows you to construct structures relating editing modes (these
could be evil modes or hydras, or anything implementing a common modal
interface ("chimera")) and manipulate which structures are active at
any point in time.  It generalizes both vim's notion of editing mode,
as well as Emacs's notion of a major mode into a unified way of
looking at things, that is, as towers of modes which can be swapped
and themselves edited using the very modes they contain.

In addition, rigpa also defines conventions that modes should follow
in order to be seamlessly integrated into editing structures.  This
includes conventions around keybindings for moving up and down the
hierarchy of editing levels, standard semantics of modifier keys,
defining a canonical action for each mode, and other such conventions
to ensure semantic uniformity across editing levels.

Installation
============

This package isn't on `MELPA <https://melpa.org/>`_ yet, but you can install a pre-release version using `straight.el <https://github.com/raxod502/straight.el>`_ by putting this somewhere in your :code:`.emacs.d`:

.. code-block:: elisp

  (use-package rigpa

    :after (evil symex)

    :straight
    (rigpa
      :type git
      :host github
      :repo "countvajhula/rigpa")

    :config
    (setq rigpa-mode t)

    ;; custom config
    (setq rigpa-show-menus nil)

    ;; navigating meta modes
    (global-unset-key (kbd "s-m"))
    (global-set-key (kbd "s-m s-m") 'rigpa-flashback-to-last-tower)
    (global-set-key (kbd "C-<escape>") 'rigpa-enter-tower-mode)
    (global-set-key (kbd "M-<escape>") 'rigpa-enter-mode-mode)
    (global-set-key (kbd "s-<escape>") 'rigpa-enter-mode-mode)
    (global-set-key (kbd "M-<return>")
                    (lambda ()
                      (interactive)
                      (rigpa-enter-selected-level)
                      (let ((ground (rigpa--get-ground-buffer)))
                        (rigpa-exit-mode-mode)
                        (switch-to-buffer ground))))
    (global-set-key (kbd "s-<return>")
                    (lambda ()
                      (interactive)
                      (rigpa-enter-selected-level)
                      (let ((ground (rigpa--get-ground-buffer)))
                        (rigpa-exit-mode-mode)
                        (switch-to-buffer ground))))
    (global-set-key (kbd "C-<return>")
                    (lambda ()
                      (interactive)
                      (rigpa-exit-tower-mode)
                      (rigpa-enter-mode-mode)))

    ;; indexed entry to various modes
    (global-set-key (kbd "s-n") 'evil-normal-state)
    (global-set-key (kbd "s-y")        ; symex mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "symex")))
    (global-set-key (kbd "s-;") (kbd "s-y"))
    (global-set-key (kbd "s-w")        ; window mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "window")))
    (global-set-key (kbd "s-v")        ; view mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "view")))
    (global-set-key (kbd "s-x")        ; char mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "char")))
    (global-set-key (kbd "s-a")        ; activity mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "activity")))
    (global-set-key (kbd "s-z")        ; text mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "text")))
    (global-set-key (kbd "s-g")        ; history mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "history")))
    (global-set-key (kbd "s-i")        ; system mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "system")))
    (global-set-key (kbd "s-b")        ; buffer mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "buffer")))
    (global-set-key (kbd "s-f")        ; file mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "file")))
    (global-set-key (kbd "s-t")        ; tab mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "tab")))
    (global-set-key (kbd "s-l")        ; line mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "line")))
    (global-set-key (kbd "s-e")        ; application mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "application")))
    (global-set-key (kbd "s-r")        ; word mode
                    (lambda ()
                      (interactive)
                      (rigpa-enter-mode "word"))))

"License"
==========
This work is "part of the world." You are free to do whatever you like with it and it isn't owned by anybody, not even the creators. Attribution would be appreciated and would help, but it is not strictly necessary nor required. If you'd like to learn more about this way of doing things and how it could lead to a peaceful, efficient, and creative world, and how you can help, visit `drym.org <https://drym.org>`_.
