; File for initial loading of gridworld code
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Note: Do not change the loading order, as some definitions in earlier
; files may be needed in later files.
(load "gridworld-definitions.lisp")
(load "gridworld-planning.lisp")
(load "go.lisp")
(load "implement-effects.lisp")
(load "simulation-and-function-evaluation.lisp")
(load "our-world.lisp")
(initialize-state-node)
