; I'm separating out this function because it causes compiler errors.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This procedure executes the first action specified by arg action-name 
;; of the best plan found in go!, updates *real-clock* and *AG-clock*, 
;; and refreshes the actual new current state *curr-state-node* after 
;; gathering new facts that may be available after the action taken.  
;; (The assumption is that some facts associated with objects at a 
;; particular point will not become known to the agent till it gets to that 
;; point, and so the new state will in general be richer in facts than the 
;; anticipated state. After executing the action, the action along with 
;; its real resulting state, real duration, expected duration, and  
;; *real-clock* and *AG-clock* is logged in *real-history* and 
;; *AG-history*, respectively.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Daphne: Revised 10/18/2012 to handle new representation of terms
; Daphne: Revised Dec. 2009 to handle new representation of wff-htable
(defun implement-effects (action-name)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
 (let*	((the-action (action-type action-name))
         (action (eval action-name)); the agent's action concept
         (expected-duration (op-time-required action))
         (name (op-name action)); name of operator instantiated by action
         (name.actual-str (concatenate 'string (string name) ".ACTUAL"))
         ; If 'name.actual' has no value yet, we assign it a copy of 
         ; (eval name) as value (i.e., the actual operator is the same 
         ; as the conceptual one); but change the op-name field.
         (name.actual 
           (if (find-symbol name.actual-str) 
               (find-symbol name.actual-str)
               ; Build the required variant.
               (name-of-actual-operator name.actual-str (eval name)))
         )
         (op.actual (eval name.actual)); presumed to exist at this point
         (pars (op-pars op.actual))
         (par-values (op-pars action))
         (is-first-iter 'T)
         (is-terminated 'NIL)
         (par 'NIL)
         (par-value 'NIL)
         (stopconds.actual (op.actual-stopconds op.actual))
         (adds.actual (op.actual-adds op.actual))
         (deletes.actual (op.actual-deletes op.actual))
         tempdeletes adds deletes stopconds implied-facts bindings 
         new-state deletes-copy new-wff-htable new-terms
        )
        ; Substitute the parameter values for the parameters in the termination 
        ; conditions and the actual effects.
        (while pars
           (setq par (pop pars))
           (setq par-value (pop par-values)) 
           (setq adds.actual (subst par-value par adds.actual))
           (setq deletes.actual (subst par-value par deletes.actual))
           (setq stopconds.actual (subst par-value par stopconds.actual)) 
        )
        ; We can look at the "local-value" of the state node previously 
        ; calculated in lookahead, since the effects contributing to 
        ; "state-value" are the same in both the model and actual versions 
        ; of each internal action operator.
        (setq *total-value* 
           (incf *total-value* (+ (op-value action) 
                                  (state-node-local-value *curr-state-node*))))
		
        ; Reset *event-timer* to start tracking duration of the selected action.
        (setq *event-timer* 0)
		
        ; Execute the action iteration by iteration while its termination 
        ; conditions are all not true in AG's KB.
        (while (or (eq 'T is-first-iter) (eq is-terminated 'NIL))
           (incf *event-timer* 1)
           (if (eq 'T is-first-iter)
               (setq is-first-iter 'NIL)
               ; For each subsequent (after first) iteration, check whether any 
               ; of the termination conditions of the selected action are true 
               ; according to AG's KB.
               (progn 
                  ;(handleExtOps)
                  ; Above line commented out only for opp (i.e., normal) runs
                  (setq stopconds (mapcar #'simplify-value stopconds.actual))
                  (if (evaluable-func (car stopconds))
                      (setq stopconds (simplify-value stopconds)))
                  (if (eq 'T 
                          (eval (cons 'memb 
                                  (list (quote 'T) 
                                        (list 'quote 
                                          (mapcar 
                                           #'(lambda (x) 
                                               (evalFunctionPredicate x)) 
                                           stopconds)))))
                      )
                      (setq is-terminated 'T))
               )
           )
           ; Evaluate and apply effects to the world KB and AG's KB only if the 
           ; selected action's termination conditions are all false according 
           ; to AG's KB.		
           (when (eq 'NIL is-terminated)
               ; Evaluate and simplify the actual adds.
               (setq adds (mapcar #'simplify-value adds.actual))
               (if (evaluable-func (car adds))
                   (setq adds (simplify-value adds))
               )				
				
               ; Evaluate and simplify the actual deletes.
               (setq deletes (mapcar #'simplify-value deletes.actual))
               (if (evaluable-func (car deletes))
                   (setq deletes (simplify-value deletes)))
               (setq deletes (set-differencef deletes adds))	
               ; Find all the bindings of variables in actual deletes so 
               ; we can make sure all actual deletes will be removed.
               (setq bindings (remove-if #'degenerate-binding 
                               (all-bindings-of-goals-to-fact-htable deletes
                                 (state-node-wff-htable *curr-state-node*)
                                 (state-node-terms *curr-state-node*))))
               (setq bindings (remove-duplicates bindings :test #'equal))			
               ; Apply all bindings found to get all actual deletes.
               (when (and (not (equal '(T) bindings)) (not (null bindings)))
                   (setq tempdeletes 'NIL)
                   (dolist (u bindings)
                       (setq deletes-copy deletes)
                       (dolist (b u)
                           (setq deletes-copy 
                               (subst (cdr b) (car b) deletes-copy)) )
                       (setq tempdeletes (unionf deletes-copy tempdeletes))
                   )			
                   (setq tempdeletes (remove-duplicates tempdeletes :test #'equal))
                   (setq tempdeletes (mapcar #'simplify-value tempdeletes))
                   (setq deletes (set-differencef tempdeletes adds))
               )		
               ; Apply actual effects to the world KB after further inferences.
               (remove_list_of_tuples_from_hashtable deletes *protected-facts* 'NIL)
               (add_list_of_tuples_to_hashtable adds *protected-facts* 'NIL)
               (setq *world-facts* (all-inferences *protected-facts* 
                                     *general-knowledge* *inference-limit*))
               (add_htable_to_hashtable *protected-facts* *world-facts* 'NIL)
				
               ; Apply actual effects to AG's KB.
               (setq new-terms (state-node-terms *curr-state-node*))
               (setq new-wff-htable (state-node-wff-htable *curr-state-node*))
               (setq new-terms 
                 (remove_term_list_from_term_list 
                   (remove_list_of_tuples_from_hashtable deletes new-wff-htable 'T)
                      new-terms))
               (setq new-terms 
                   (merge_term_list_with_term_list 
                      (add_list_of_tuples_to_hashtable adds new-wff-htable 'T)
                      new-terms))
               (setf (state-node-terms *curr-state-node*) new-terms)
							
               ; Modify *curr-state-node* to update the agent's beliefs 
               ; in light of the locally evident facts (and beliefs that 
               ; have evidently become false).
               (setq new-state (copy_construct_hashtable 
                                   (notice-new-local-facts *curr-state-node*)))	
               (setq *states* (cons new-state (cdr *states*)))

               ; After executing for an iteration, the action along with its
               ; real resulting state, real elapsed duration, expected duration, 
               ; and *real-clock* is logged in *real-history*.
               (push (list (cons the-action new-state) *event-timer* 
                           expected-duration *real-clock*) *real-history*)
						
               ; After the very first iteration of execution, the action along 
               ; with its real resulting state, real elapsed duration, expected 
               ; duration, and *AG-clock* is logged in *AG-history*.  Only the 
               ; start and (later) the end, and not each iteration of execution, 
               ; of the action will be logged in *AG-history* to reflect that 
               ; AG does not know in advance what the real duration of an action
               ; will surely be.
               (if (= 1 *event-timer*)
                   (push (list (cons the-action new-state) 
                               *event-timer* expected-duration *AG-clock*) 
                         *AG-history*)) 
           )
           ; Increment *real-clock* and *AG-clock* after each iteration of 
           ; execution.
           (incf *real-clock* 1)
           (incf *AG-clock* 1)
        ); end of executing the action iteration by iteration

        ; Log the end of the action in *AG-history* to indicate AG's awareness 
        ; of its termination.
        (when (> *event-timer* 2)
            (push (list (cons the-action new-state) (- *event-timer* 1) 
                        expected-duration (- *AG-clock* 2)) *AG-history*)
        )
 )
); end of implement-effects 

