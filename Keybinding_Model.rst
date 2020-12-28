Keybinding Model
================

Spatial
-------
	← ↓ ↑ →  h j k l
	
	> < + - expand contract

Temporal
--------
	Undo/redo u C-r

Modifiers
---------
Verb
^^^^
	S-

Superlatives
^^^^^^^^^^^^
	More/most C- / M-

Adverbs
^^^^^^^
	C-

	M- [where is this an adverb? or is it proposed as one?]

	S- [e.g. "fine" scroll in View; cases where C- or M- is being used as a superlative]

Meta
^^^^
	H-

	H-m toggle menu

	H-h toggle feedback

Composition / decomposition
---------------------------
	Join / split J |

CRUD
----
	\+ create

	x delete

	c change

	y copy

	p paste

Entry/Exit
----------
	Local entry/exit: Enter/Escape

	Direct entry/exit: s-<key>

Concepts
--------
	"Canonical Action":
		- Each mode has a "canonical" action which is mapped to the same keybinding as direct entry (e.g. s-b s-b, or s-w w)
		- Canonical action usually (always?) exits the mode
		- E.g. window: maximize, buffer: flashback/"alt-tab", view: recenter, symex: evaluate
	"Default":
		= return settings to some notion of "default" (e.g. window: equal split, view: normal zoom, symex: indent)
	The base (unmodified) keybindings set may include any higher-level keybindings (e.g. ? which would also fall under H-)

	Simplicity over consistency.

Meta
----
	Esc/Enter for higher/lower modes at a particular level

	C-Esc/Enter for meta-level by level (base, mode, tower, ...) ["sideways"]

	M-Esc to create a new ground and is otherwise equivalent to C-Esc ["vertically"]

	s-Enter returns to (local) ground

	H-Enter returns to absolute ground

Misc
----
	/ Search

	m ' Mark / return to mark (e.g. mark a buffer to return to, or a line number, or a window, or a view zoom level, etc.)

	? About/help (e.g. lookup word in dictionary, lookup identifier in docs, buffer info, etc.)
