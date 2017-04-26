;; SEPARATED OUT BECAUSE IT CAUSES TROUBLE WHEN COMPILED -LKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This procedure chains forward from the *curr-state-node*, using some 
;; fixed (or user-set) search beam.  It reports the seemingly best plan 
;; (*plan*) and corresponding state sequence (*states*); then it executes 
;; the first step of that best plan, updating *curr-state-node*, *plan*
; and *states*.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun go! ( ) ; Revised Dec. 2009
        (let (poss-actions step action-name old-wffs old-terms)

(format t "~% GO ~%")
                ;Handle the spontaneous fire and flood.
                ;(handleExtOps)
                ; Above line commented out only for opportunic runs


                (setq poss-actions
                 (chain-forward *curr-state-node* *search-beam*))

                (when (null poss-actions)
                      (incf *AG-clock* 1)
                      (incf *real-clock* 1)
                      (return-from go! "NO MORE ACTIONS POSSIBLE!")
                )
 (format t "~%~%POSSIBLE ACTIONS & VALUES: ~a" poss-actions)
 (format t "~%SEEMINGLY BEST PLAN: ~a" *plan*)

                ; Reset *curr-state-node* to the first (leftmost) successor.
                (setq old-wffs (state-node-wff-htable *curr-state-node*))
                (setq old-terms (state-node-terms *curr-state-node*))
                (setq *curr-state-node*
                     (eval (cdar (state-node-children *curr-state-node*))) )
                (setf (state-node-terms *curr-state-node*) old-terms)
                (clrhash (state-node-wff-htable *curr-state-node*))
                (add_htable_to_hashtable old-wffs
                        (state-node-wff-htable *curr-state-node*) 'NIL)

                (setq step (pop *plan*))
 (format t "~%~%STEP TO BE TAKEN: ~a" step)
 (format t "~%EXPECTED STATE; ~%  ~a" (second *states*))
                (pop *plan*)
                ; Remove the previous state and bring to the fore the expected 
                ; current state.
                (pop *states*)

    ; Reset the "successor actions already explored" to nil,
    ; because otherwise we won't take account of possibilities.
    ; engendered by newly discovered local facts.                    
                (setf (state-node-operators *curr-state-node*) nil)
                (setf (state-node-children *curr-state-node*) nil)
    ; LKS, Sep 28/12: To free memory, reset grandparent pointer to nil:
        (if (state-node-parent *curr-state-node*); (action-name . state-node-name)
            (if (state-node-parent (eval (cdr (state-node-parent *curr-state-node*))))
                (setf (state-node-parent (eval (cdr (state-node-parent *curr-state-node*))))
                      nil)))

                ; Actually implement the effects of the action in the world.
                (setq action-name (car (state-node-parent *curr-state-node*)))
                (setq *node-now* *curr-state-node*)
                (implement-effects action-name)
        )
); end of go!

