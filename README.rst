rigpa
=====

A modal UI framework for Emacs.

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

Watch the demo from EmacsConf 2020:

.. raw:: html

  <p align="center">
    <a href="https://www.youtube.com/watch?v=jBUurG3f_aM">
      <img width=70% src="https://i.imgur.com/EV3q6G7.png" alt="Watch video" title="Watch video"/>
    </a>
  </p>

Installation
============

This package isn't on `MELPA <https://melpa.org/>`_ yet, but you can install a pre-release version using `straight.el <https://github.com/raxod502/straight.el>`_ by putting this somewhere in your :code:`.emacs.d`:

.. code-block:: elisp

  (use-package rigpa

    :after (evil symex buffer-ring)

    :straight
    (rigpa
      :type git
      :host github
      :repo "countvajhula/rigpa")

    :config
    (setq rigpa-mode t)

    ;; temporary workaround for https://github.com/countvajhula/rigpa/issues/9
    (remove-hook 'evil-symex-state-exit-hook #'symex-disable-editing-minor-mode)

    ;; custom config
    (setq rigpa-show-menus nil)

    ;; navigating meta modes
    (global-unset-key (kbd "s-m"))
    (global-set-key (kbd "s-m s-m") 'rigpa-flashback-to-last-tower)
    (global-set-key (kbd "C-<escape>")
                    (lambda ()
                      (interactive)
                      (when (eq rigpa--complex rigpa-meta-complex)
                        (rigpa-exit-mode-mode))
                      (rigpa-enter-tower-mode)))
    (global-set-key (kbd "M-<escape>") 'rigpa-enter-mode-mode)
    (global-set-key (kbd "s-<escape>") 'rigpa-enter-mode-mode)
    (global-set-key (kbd "M-<return>")
                    (lambda ()
                      (interactive)
                      (when (eq rigpa--complex rigpa-meta-complex)
                        (rigpa-enter-selected-level)
                        (let ((ground (rigpa--get-ground-buffer)))
                          (rigpa-exit-mode-mode)
                          (switch-to-buffer ground)))))
    (global-set-key (kbd "s-<return>")
                    (lambda ()
                      (interactive)
                      (when (eq rigpa--complex rigpa-meta-complex)
                        (rigpa-enter-selected-level)
                        (let ((ground (rigpa--get-ground-buffer)))
                          (rigpa-exit-mode-mode)
                          (switch-to-buffer ground)))))
    (global-set-key (kbd "C-<return>")
                    (lambda ()
                      (interactive)
                      (when (eq rigpa--complex rigpa-meta-tower-complex)
                        (rigpa-exit-tower-mode)
                        (rigpa-enter-mode-mode))))

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

Usage
=====
"Direct entry" into modes is (by default) done via the "super" key prefix, e.g. ``s-v`` goes into View Mode. ``Esc`` and ``Enter`` will always return you to a tower-native state and also navigate that tower. If you leave a buffer or window while in some state, by momentarily entering buffer or window mode, you will be placed back in your original state when you return. In-buffer states like Symex, Word, Character mode are backed by Evil, and do not show menus. Global states like View, Window, Buffer, are hydra-backed and can show/dismiss menus on demand (default binding ``H-m``).

The most useful towers at the moment are Vim tower, Lisp tower (containing Symex mode), and Emacs tower. If you are working with Lisp code, then alternating (e.g. via ``s-m s-m``) between Vim and Lisp towers, or between Emacs and Lisp towers, is a common usage pattern. Whatever towers you define, you will probably want to leverage direct entry into View, Window, Buffer modes as part of normal usage, especially for their canonical actions. E.g. ``s-b s-b`` to alternate to most recent buffer, ``s-w s-w`` to alternate to most recent window, ``s-v <tab>`` to set to preferred zoom, ``s-w w`` to maximize window, besides using the usual navigation commands in these modes to get around.

See the `Keybinding Model <https://github.com/countvajhula/rigpa/blob/master/Keybinding_Model.rst>`_ for keys that work in every mode, including navigations for getting around, and transformations for moving things around, or deleting, transforming them in some way.

Tips
====

Mode Line Feedback
------------------

Rigpa comes with many modes -- View mode, Window mode, Buffer mode, Line mode, and more. To keep track of which mode you're in at any time, it's useful to have visual feedback in the form of a color-coded mode indicator in your mode line, with colors that are different enough to give you a clue at a glance whether you're in the right mode. Rigpa currently defines and maintains Evil states corresponding to every Rigpa mode in order to benefit from existing Emacs tools that have already been written to track your Evil state. So you can use any of these existing plugins for this purpose, including powerline and `telephone-line <https://github.com/dbordak/telephone-line>`_. For example, you could use `this config for telephone-line <https://github.com/countvajhula/.emacs.d/blob/95b43a92cabf7719875f1f937b4c3de093d32a24/my-init/application-environment.el#L437-L522`__ to ensure the different modes have distinct and useful colors.

"License"
==========
This work is "part of the world." You are free to do whatever you like with it and it isn't owned by anybody, not even the creators. Attribution would be appreciated and would help, but it is not strictly necessary nor required. If you'd like to learn more about this way of doing things and how it could lead to a peaceful, efficient, and creative world, and how you can help, visit `drym.org <https://drym.org>`_.
