; "go.lisp" and "implement-effects.lisp" are deliberately excluded
; from compilation, because compiling them slows down or breaks 
; execution for unknown reasons.
(compile-file "gridworld-definitions.lisp")
(compile-file "gridworld-planning.lisp")
(compile-file "simulation-and-function-evaluation.lisp")
(compile-file "gridworld-world.lisp")

