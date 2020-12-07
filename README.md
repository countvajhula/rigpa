# rigpa
Modular editing levels and towers for Emacs

# Description
Rigpa allows you to construct structures relating editing modes
(in principle, these could be evil modes or hydras, or anything
implementing a common modal interface ("chimera")) and manipulate which
structures are active at any point in time.  It generalizes both vim's
notion of editing mode, as well as Emacs's notion of a major mode
into a unified way of looking at things, that is, as towers of
modes which can be swapped and themselves edited using the very
modes they contain.

In addition, rigpa also defines conventions that modes
should follow in order to be seamlessly integrated
into editing structures.  This includes conventions around keybindings
for moving up and down the hierarchy of editing levels, standard
semantics of modifier keys, defining a canonical action for each
mode, and other such conventions to ensure semantic uniformity across
editing levels.
