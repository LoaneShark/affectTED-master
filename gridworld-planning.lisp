;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Name: gridworld-planning.lisp
;; 
;; Author: Lenhart Schubert and Daphne Liu
;; Date of Version 1: Jan. 2008 version 1 by Lenhart Schubert
;; Date of Version 2: Apr. 2009 version 2 by Daphne Liu
;; Date of Version 3: Jan. 2010 version 3 by Daphne Liu
;; Date of Version 4: Nov. 2010 version 4 by Daphne Liu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ABOUT KNOWLEDGE STORAGE AND AVAILABILITY TO 'AG':
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; We distinguish 3 sorts of facts
;
; - roadmap facts (about the arrangement of points and roads
;   connecting them), stored as the value of *roadmap-knowledge*;
;   they are created via the function 'def-roadmap'; the agent
;   AG is assumed to know these facts;
;
; - general facts about the world, in particular about the
;   properties of various types of entities, stored as the value
;   of *general-knowledge*; these are non-unit and/or non-ground
;   Horn clauses, typically created by 'def-object', though they
;   could also be created by explicitly pushing facts into 
;   *general-knowledge*; they are intended to allow further 
;   inferences to be drawn about the current situation, either 
;   from the perspective of the agent's beliefs (assuming the 
;   agent AG knows all the general facts), or for modelling the 
;   world as it actually is.
;
; - specific facts, i.e., ground predications (we might at some
;   point consider allowing negative ground literals as well);
;   originally sets of these were associated with particular
;   points in the road grid, but with the introduction of a
;   clear distinction between what the agent believes, and
;   what it will "notice" at a particular location, this scheme
;   has had to be changed; specific facts are now stored 
;   uniformly as *world-facts*; what the agent can observe at
;   a particular location, after each action, is separately
;   calculated, using the notion of objects that are "co-moving"
;   with another object. For example, suppose there is an apple
;   at the 'home' location initially, and the agent decides
;   to pocket it, despite not being hungry at that point. If
;   the agent now walks to another location, the facts *about*
;   the apple -- that the agent has it, that it's edible, etc.,
;   should be available at the new location -- not just as facts
;   the agent knows (which was the case in the old version) but
;   as actual facts that the actual implementation of the agent's
;   actions at that location can modify (e.g., making the apple
;   inedible, and rendering false that the agent "has" it, by 
;   eating it). We could perhaps handle this by moving facts
;   about about an object from one location to another, when the
;   object moves -- but this reqquires inference in general, and
;   seems quite cumbersome. So instead, we keep specific facts
;   in the single *world-facts* repository. (This would also make 
;   possible in future to make inferences in the world that
;   depend on facts about objects in different locations -- e.g.,
;   telephone calls, remote bank deposits, social relationships
;   between non-colocated individuals, etc.) As mentioned, the
;   new scheme requires figuring out what objects are at a given
;   location, taking account of "co-moving" objects, and then
;   making non-occluded predications about the objects at the
;   agent's location noticeable to it (so that its belief can
;   be appropriately updated). We do this with the help of user-
;   supplied list of predicates *left-comoving-preds* and *right-
;   comoving*preds*, examples of which are (is_in x y) (if x is
;   in y, then x moves with y) and (has x y) (if x has y, then
;   y moves with x). 
;
;   To deal with inferred facts, we also maintain a subset of
;   *world-facts* called *protected-facts*, which includes no
;   inferred facts.
;  
; What facts are known to AG in a given state?
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; It is important to note that atomic ground facts also get
; stored in state-nodes (generated in the planning process),
; as the value of the 'wffs' field of a state-node. THESE WFFS
; CAN BE VIEWED AS THE GROUND FACTS THAT AG IS AWARE OF IN THAT
; STATE. These in general include both the roadmap facts and 
; local facts (acquired by observation at the present location
; and at previously visited locations), the latter quite possibly
; elaborated through the use of general knowledge. Note that since
; state wffs are propagated from state-to-(successor)state when 
; an operator is applied -- except for changes made by that operator 
; -- THE AGENT, AG, NEVER "FORGETS" ANYTHING IT HAS LEARNED BY 
; VISITING A POINT OR MAKING INFERENCES (except, once again, facts
; that are changed by the actions it has taken or is considering). 
;
; There is also provision for supplying some initial knowledge to
; the agent that is non-local (and not roadmap knowledge). This
; is done simply by supplying any facts we want the agent to
; know initially as part of the curr-facts argument when we
; place 'AG' at a particular initial location.
; 
; The above description of how the knowledge of AG evolves is 
; not completely accurate, because allowance is also made for 
; the possibility that the "actual" actions taken by the agent 
; have somewhat different effects from those the agent "thinks"
; they have, and also that there may be exogenous change. In 
; particular, the agent's knowledge state after performing an 
; action is recomputed (using 'notice-new-local-facts') by
; checking the agent's beliefs (current state) against locally 
; observable facts. The latter are obtained by a variant
; of the action chosen by the agent, whose name adds the
; extension '.actual' to the name of the model action assumed
; by the agent. This "actual" version may differ in its effects
; from the model version (though if no actual version is
; prespecified, a copy of the model version is used as the
; actual action). Positive locally observable facts missing
; from the agent's beliefs are added, and beliefs that would be
; locally observable if they were true, but are not in fact 
; locally present are deleted. Any new inferences that follow 
; by using *general-knowledge* are added to the *world-facts*,
; (while previous inferences are dropped), and so the agent
; may pick up some of these as well, if they are local and not
; occluded.
;
; How do we handle interaction with the user?
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; The default method is simply to have a function for querying the
; agent's current state, as a way of asking about its beliefs.
;
; But it would be nice to also be able to tell it something, and
; indeed to be able to "converse" with it -- and have it be aware
; THAT it is conversing, and to choose to reply voluntarily, rather
; than by unconscious, unnoticed "reflex".
;
; We could first of all have a 'listen!' function (optionally
; used prior to a 'go!') that allows us to provide certain
; questions, facts, or requests. This should directly insert the
; fact that the speech act occurred into the agent's belief state,
; and run relevant inferential updates based on that fact. The
; chief inference for a question might be that the question-asker
; wants AG to tell him/her/it an answer to the question (this is
; a simplification from a more general speech act approach); this
; will prime the agent for potentially responding accordingly. The
; agent will need to have operators for responding (i.e., satisfying
; its interlocutor's requests, or accepting knowledge). For 
; answering a question, it might have an operator 'answer-question',
; whose arguments are an interlocutor and a question. (We want 
; to potentially allow different behaviors towards different 
; interlocutors). This operator would have to handle both the 
; derivation of an answer and the verbal output. Is this even 
; possible within a non-hierarchical planning framework?
; 
; We should be able to handle this through computable effects:
; the effect is something like (say AG (answer-to? ?x)),
; where ?x is bound to the input question, and which upon evalu-
; ation becomes (say AG <some specific proposition>). Besides
; storing this effect in the agent's knowledge state as usual, 
; we would also ensure that if the questioner is the user, the
; answer is actually printed to standard-output as a side-effect
; when executing 'answer-to?'. At this point the agent should also
; make the inference that the interlocutor knows the content of
; the answer, and (perhaps!) believes that content. This should be 
; automatic from applying general knowledge to local facts that 
; are noticed -- and certainly facts about the user (like those 
; about the agent, AG) should be regarded as always being local 
; to wherever the agent is. So the one thing required here (in 
; addition to an appropriate 'answer-to?' function, of course) 
; is that facts about the user always be treated as local in 
; 'notice-new-local-facts'.
;
; 'Answer-to?' should handle both yes-no and wh-questions. The 
; unifications that check what actions are possible in a given
; state should be able to distinguish speech act descriptions that
; specify a yes-no question from ones that specify a wh-question,
; or for that matter, from ones that specify a direct request or 
; a fact. The interactive 'listen!' function should look at the 
; form of the input and decide whether to insert
;   (requests-answer User ?question), or
;   (requests-action User ?action), or
;   (offers-fact User ?fact),
; with appropriate values of the variables, of course.  
;
; ABOUT PROCEDURALLY EVALUABLE (OR AT LEAST SIMPLIFIABLE) PREDS/FUNCS:
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Evaluable predicates and functions must either have names
; ending in "?", or be one of +,-,*,/,<,<=,=,>=,>.
;
; Some programs may allow for equality/inequality literals of form 
; (EQ t1 t2), or (NEQ t1 t2), but this is not used here. Instead,
; we assume that in instantiating an operator, the bindings of
; distinct variables must be distinct. However, EQ and NEQ could
; be handled using corresponding evaluable predicates eq?, neq?.
;

;; SOME GLOBAL ENTITIES
;; ====================

; Note that *world-facts* are initialized in gridworld-definitions.lisp;
;~~~~~~~~~~~~~~~~~~~~~~~~ these are ALL the specific (atomic) facts
; Note also, *general-knowledge* is defined in gridworld-definitions.lisp;
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  for now, general facts = general knowledge;

(defvar *plan* nil); shows the currently best sequence of future actions
;~~~~~~~~~~~~~~~~~~

(defvar *states* nil) ; sequence of states (where each state is a set of
;~~~~~~~~~~~~~~~~~~~~   ground predications), starting at present, 
;                       corresponding to *plan*;

(defvar *inference-limit* 2) ; depth of forward inference;
;~~~~~~~~~~~~~~~~~~~~~~~~~~

;(defvar *curr-state-node* nil)

(defvar *real-history* nil) ; the sequence of actions (with parameter values)
						    ; and events so far in the world

(defvar *AG-history* nil) ; the sequence of actions (with parameter values)
						  ; taken so far from the agent's perspective

(defvar *operators* nil) ; names of the available operators;
                         ; must be set by the user

(defvar *search-beam* nil) ; list of items, each of form (i . op-names),
;~~~~~~~~~~~~~~~~~~~~~~~~~ ; where i > 0 and op-names lists the op. names
                           ; allowed at each step (to be set by the user)

;; SOME BASIC UTILITIES
;; ====================

(defun put (atm indic val) (setf (get atm indic) val))
;~~~~~~~~~~~~~~~~~~~~~~~~~

(defun unionf (x y) (union x y :test #'equal))
;~~~~~~~~~~~~~~~~~~

(defun memberf (x y) (member x y :test #'equal))
;~~~~~~~~~~~~~~~~~~

(defun intersectionf (x y) (intersection x y :test #'equal))
;~~~~~~~~~~~~~~~~~~~~~~~~~

(defun set-differencef (x y) (set-difference x y :test #'equal))
;~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun removef (lst1 lst2)
	(let ((result lst2))
		(dolist (x lst1)
			(setq result (remove x result :count 1 :test #'equal))		
		)
		result
	)
)

(defun var (x) ; is x a variable, i.e., an atom with first character"?"
;~~~~~~~~~~~~~
 (if (and x (symbolp x))
     (char= (nth 0 (coerce (string x) 'list)) #\?)
     nil ))

(defun *append (u v); append 2 lists where one or both may be T,
;~~~~~~~~~~~~~~~~~~~; interpreted here as trivial unifier, hence,
                    ; like the empty list
   (if (equal u T) v (if (equal v T) u (append u v))) )


(defun first-n (x n)
;~~~~~~~~~~~~~~~~~~~~
; Return the length-n prefix of x
  (if (atom x) x (butlast x (max 0 (- (length x) n)))) )
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;; ROUTINES FOR FINDING THE BINDINGS THAT MATCH GOALS TO A STATE ;;
;; ============================================================= ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun all-bindings-of-goals-to-fact-htable (goals fact-htable terms); Revised Dec. 2009
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Find all unifiers of the variables occurring in goals (a set
; of +ve & -ve literals, possibly containing variables) obtainable
; by matching the goals to the fact-htable (a set of +ve ground literals);
;
; Method: Here we just reorder the goals and extract all terms
;    occurring in 'fact-htable', and then call the recursive routine
;    (all-bindings-of-goals-to-fact-htable1 goals fact-htable terms) -- see
;    this for further explanation.
;
	(let ((gg goals) (ff fact-htable) 
		  (wff-terms (remove-duplicates terms :test #'equal))
		 )
   ; Reorder goals so that positive goals come before negative
   ; goals, and for same-sign goals, goals with more variables 
   ; precede ones with fewer variables, and for same-sign, same-
   ; number-of-variables goals, goals with more arguments precede 
   ; ones with fewer arguments; this is to minimize work in matching;
   ;
		(setq gg (sort (copy-list gg) #'> :key #'rank-for-goal-sorting))
   		(all-bindings-of-goals-to-fact-htable1 gg ff wff-terms) 
	)
); end of all-bindings-of-goals-to-fact-htable


(defun rank-for-goal-sorting (goal)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; The idea is to give highest ranks to goals that will tend to
; cut down the search space most quickly (i.e., have few matches).
; Evaluable expressions are given lowest rank, because (currently)
; goals containing evaluable expressions must be simplified to
; ones not containing such expressions (i.e., T, nil, or a goal
; containing only ordinary, nonevaluable preds/functions) before
; being matched to the "facts" of a given state.
;
; Assign a rank to 'goal' so that all positive goals have a higher
; rank than all negative goals, and among goals of the same sign,
; goals with fewer variables always have a higher rank than goals
; with more variables, and among goals of the same sign with the
; same number of variables, goals with more terms always have 
; higher rank than goals with fewer terms. Assume that a predicate
; can have no more than 10 arguments (o/w ranks won't be exactly
; as stated). Finally lower the rank by a large amount if the goal
; contains an evaluable expression, so that its rank will be lower
; than that of any goal containing no evaluable expressions.
;
	(let ((rank (if (poslit goal) 200 0))
		(var-count (length (remove-duplicates (vars goal))))
		(term-count+1 
		  (if (poslit goal) (length goal) (length (second goal)))))
		(decf rank (* 10 var-count))
		(incf rank term-count+1) 
		(when (contains-evaluable-expr goal)
		  (decf rank 400)
		)
		rank 
	)
); end of rank-for-goal-sorting


(defun contains-evaluable-expr (expr)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	(cond	((atom expr) nil)
        	((evaluable-func (car expr)) t)
        	((atom (car expr)); for atomic (car expr), need only check cdr
         	(if (member t (mapcar #'contains-evaluable-expr (cdr expr)))
             	t nil))
        	(t (if (member t (mapcar #'contains-evaluable-expr expr))
            	t nil))
	)
); end of contains-evaluable-expr
   

(defun all-bindings-of-goals-to-fact-htable1 (goals fact-htable terms); Revised Dec. 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Find all unifiers of the variables occurring in goals (a set
; of +ve & -ve literals, possibly containing variables) obtainable
; by matching the goals to the fact-htable (a hashtable of 
; (+ve) ground literals), using 'terms' as the names of all 
; possible individuals (these are just the *distinct* terms 
; occurring in 'fact-htable', precomputed for convenience).

; Method: 
;   1. results := nil; {initialization}
;   2. uu := all-bindings-of-goal-to-fact-htable(first(gg),fact-htable,terms);
;   3. if rest(gg) = nil then return uu;
;   4. if uu = (t) (first goal matched exactly)
;      then return all-bindings-of-goals-to-fact-htable1
;                           (rest(gg), fact-htable, terms);
;   5. For each u in uu:
;      i. vv := all-bindings-of-goals-to-fact-htable1
;                           (rest(gg)_u, fact-htable, terms),
;               where subscript _u indicates substitution using u;
;     ii. if vv = nil then continue with next u in uu;
;    iii. append to 'results' all the unifiers obtained by
;         grafting u into the unifiers in vv;
;   6. return results.
;
	(prog ((gg goals) g results uu vv)
		(setq g (pop gg))
		(setq uu (all-bindings-of-goal-to-fact-htable g fact-htable terms))
		(when (null gg) (return uu))
		(when (equal uu '(t)) 
           (return (all-bindings-of-goals-to-fact-htable1 gg fact-htable terms)) 
		)
		(dolist (u (reverse uu)); just to end up with original order
			(setq vv (all-bindings-of-goals-to-fact-htable1
                      (subst-unifier-in-wffs u gg) fact-htable terms))
           	(when vv 
           		(setq vv (mapcar #'(lambda (v) (*append u v)) vv))
				(setq results (append vv results))
			)
		)
		(return results)
	)
); end of all-bindings-of-goals-to-fact-htable1


(defun all-bindings-of-goal-to-fact-htable (g ff terms); Revised Dec. 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	(if (poslit g)
		(all-bindings-of-posgoal-to-fact-htable g ff)
		(all-bindings-of-neggoal-to-fact-htable g ff terms) 
	)
)

(defun all-bindings-of-posgoal-to-fact-htable (g ff); Revised Dec. 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Before attempting to find the bindings of goal g to the facts ff,
; check if g contains an evaluable expression & if so simplify it.
; If the simplification yields T, then the list '(T) containing
; just the trivial unifier is returned (because "truth" matches
; any set of facts); if simplification yields nil, then the empty
; list () of unifiers is returned (because "falsity" matches no
; set of facts). If the simplified goal still contains an evaluable
; expression, then () is again returned (because we don't handle
; solving for variables embedded by computable functions). After 
; these special cases have been treated, the unifier of (the 
; possibly simplified) goal with each fact is found, and the
; non-nil values are returned.
	(prog ((g1 g))
       (when (contains-evaluable-expr g1)
             (setq g1 (simplify-value g1))
             (if (eq g1 t) 
             	(return '(t))
                (when (or (eq g1 nil) 
                          (contains-evaluable-expr g1))
					(return nil) 
                )
             )
       )
       (return (remove-if #'null 
                 (mapcar #'(lambda (f) (unifier g f)) 
                 	(possible-positive-unifiers g ff))
               )
       )
	)
); end of all-bindings-of-posgoal-to-fact-htable 
 
(defun possible-positive-unifiers (g ff); Revised Dec. 2009 by Daphne Liu
	(let* ((shortest-leng 0) hash-value hash-leng
	       shortest-hash-value (shortest-index  -1)
		   (keys (generate_allkeys_from_hashkey (convert_pred_to_hashkey g)))
		   (keys-leng (length keys))
		   (first-non-null-hash-value-reached 'NIL)
		  )
		  
		 (dotimes (i keys-leng)
		 	(setq hash-value (gethash (nth i keys) ff))
		 	(when (not (null hash-value))
			 	(setq hash-leng (car hash-value))
			 	(when (and (> hash-leng 0) 
			 			   (or (> shortest-leng hash-leng) (null first-non-null-hash-value-reached))
			 		  )
			 		(setq shortest-leng hash-leng)
			 		(setq shortest-index i)
			 		(setq shortest-hash-value (cdr hash-value))
			 		(setq first-non-null-hash-value-reached 'T)
			 	)
		 	)
		 )		
		 
		 shortest-hash-value
	)
)


(defun all-bindings-of-neggoal-to-fact-htable (g ff terms); Revised Dec. 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	(complement-unifiers 
       (all-bindings-of-posgoal-to-fact-htable (second g) ff) (vars g) terms
    )
)

(defun alphabetically-order (uu); alphabetically order variables in
								; the unifiers listed in uu
	(mapcar #'(lambda (u) 
              (if (eq u t) t
                  (sort (copy-list u) #'string< 
                        :key #'(lambda (x) (string (car x))) )))
          uu 
	)
)


(defun complement-unifiers (uu vars terms)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Find the bindings of 'vars', with elements of 'terms' as possible 
; binders, that DON'T coincide with any bindings in 'uu'.
;
; Method in the general case: subtract uu from the set of all bindings
; (where we're sure the subtraction works because the order of variables
; in unifiers is first made uniform).
;
; The elements of uu are generally of form ((var1 . val1) ... 
; (vark . valk)), where the var1, ..., vark are the same (and in the 
; same order) in all cases. However, it is possible that uu is nil
; while the given 'vars' are nonempty, in which case all possible
; bindings of the 'vars' should be returned -- that's why the 'vars'
; are separately supplied. Also uu might be (t) (indicating that the
; positive form of a negative ground literal was matched to a state),
; in which case the result should be nil.
;
	(when (null vars); then uu must be nil or '(t)
     	(return-from complement-unifiers (if (null uu) '(t) nil)) 
    )
	(when (null terms); unexpected condition
     	(return-from complement-unifiers nil)
    )
	(let (ordered-vars ordered-uu vv)
		; we keep variables in lexicographic order, so that set-
		; differencing will work (e.g., the set-difference between
		; (((?x . a) (?y . b))) & (((?y . b) (?x . a))) should be nil)
		(setq ordered-vars 
            (sort (copy-list vars) #'string< 
                                   :key #'(lambda (x) (string x))))
		(setq ordered-uu (alphabetically-order uu))
		(setq vv (all-bindings ordered-vars terms))
		; `all-bindings' keeps the variables in the given order in 
		; the list of unifiers produced as output;
		(reverse (set-differencef vv ordered-uu))
	)
); end of complement-unifiers

          
(defun all-bindings (vars terms); cons each var with each term
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	(let (vv ww)
		(cond	((and vars terms)
           			(setq vv (mapcar #'(lambda (x) (cons (car vars) x)) terms))
           			(setq ww (all-bindings (cdr vars) terms))
           			(combine-sets-of-unifiers (mapcar #'list vv) ww)
           		)
          		(t nil)
        )
	)
); end of all-bindings
 

(defun combine-sets-of-unifiers (uu vv);
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; concatenate each unifier in uu with each unifier in vv;
; the variables in uu will precede those in vv;
	(when (null uu) (return-from combine-sets-of-unifiers vv))
	(when (null vv) (return-from combine-sets-of-unifiers uu))
	(let (result)
		(dolist (u (reverse uu))
			(setq result 
				(append (mapcar #'(lambda (v) (*append u v)) vv) result) 
			)
		)
  		result 
  	)
); end of combine-sets-of-unifiers
    

;; The following two programs ignore the possibility of EQ, NEQ
;; literals for the time being..

(defun poslits (lits); all positive lits among 'lits'
;~~~~~~~~~~~~~~~~~~~~~
	(remove-if-not #'poslit lits)
)


(defun neglits (lits); all negatively embedded atoms among 'lits'
;~~~~~~~~~~~~~~~~~~~~~
	(mapcar #'second ; drop "not"s 
         (remove-if-not #'neglit lits))
)

(defun poslit (lit) (not (neglit lit)))
;~~~~~~~~~~~~~~~~~~

(defun neglit (lit) (and (listp lit) (eq (car lit) 'not)))
;~~~~~~~~~~~~~~~~~~ 

(defun collect-terms (lits); all terms occurring in literals 'lits'
;~~~~~~~~~~~~~~~~~~~~~~~~~~~
	(remove-duplicates ;; NB: we use :test #'equal for generality
		(apply #'append (mapcar #'args lits)) :test #'equal)
)

(defun collect-terms-duplicate (lits); all terms occurring in literals 'lits'
;~~~~~~~~~~~~~~~~~~~~~~~~~~~
	(apply #'append (mapcar #'args lits))
)

(defun vars (lit); bag of variables occurring in literal 'lit'
;~~~~~~~~~~~~~~~~~
	(if (atom lit) nil (remove-if-not #'var (args lit)))
) 

(defun collect-vars (lits); set of vars occurring in literals 'lits'
;~~~~~~~~~~~~~~~~~~~
	(remove-duplicates (apply #'append (mapcar #'vars lits)))
)

(defun args (lit); return list of args occurring in literal 'lit'
;~~~~~~~~~~~~~~~~~~~
	(cond 	((atom lit) nil)
	   		((eq (car lit) 'not) (cdr (second lit)))
	   		(t (cdr lit)) 
	)
)

(defun find-all-positive-bindings (poslits db); 
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Find all sets of bindings of the variables occurring in poslits (a
; set of positive literals) obtainable by matching poslits to the db
; (a set of positive ground literals);
;
	(prog (results phi u remlits vv)
       		(if	poslits 
       			(setq phi (car poslits)) 
                (return '(T)); T is the trivial unifier
            )
       		(dolist (phi1 (if	(equal (car phi) 'EQ)
                         		(equalities db)
                         		db 
                          )
                    )
          		(setq u (unifier phi phi1))
          		(when u
             		(setq remlits 
             			(mapcar #'(lambda (x) (subst-unifier u x))
                                     (cdr poslits))
                    )
             		(setq vv (find-all-positive-bindings remlits db))
            		(when vv
                 		(setq results
                    		(unionf (mapcar #'(lambda (v) (*append u v)) vv) 
                    			results)
                    	)
                    )
                )
			)
			(return results)
	)
); end of find-all-positive-bindings

(defun equalities (db);
;~~~~~~~~~~~~~~~~~~~~~~
; Find all equalities of form (EQ c c) where c is some constant appearing
; in db (a set of positive, function-free ground literals).
	(let ((constants (remove-duplicates (reduce #'append (mapcar #'cdr db)))))
	
		(mapcar #'(lambda (x) (list 'EQ x x)) constants) 
	)
)

(defun unifier (lit1 lit2);
;~~~~~~~~~~~~~~~~~~~~~~~~~
; Unify two literals (where `lit2' for our purposes is ground),
; if possible, returning the unifier if it exists and nil otherwise. 
; For equal ground literals, the unifier is T, else it is a list 
; ((var1 . term1) ... (vark . termk)). Variables are NOT renamed,
; i.e., a variable occurring in both lit1 and lit2 will be uniformly 
; bound to a unique term. Variables are expected to be Lisp symbols
; starting with `?'. Substitution for variables of lit1 is preferred
; to substitution for variables of lit2 (allowed for generality).
	(if (not (equal (car lit1) (car lit2))) 
  		nil ; must be same pred
      	(if (equal (cdr lit1) (cdr lit2)) 
      		T ; trivial unifier
          	(if (equal (car lit1) 'not) 
				(unifier (second lit1) (second lit2))
				(if (not (equal (length lit1) (length lit2))) 
					nil
					((lambda (x) (if (null x) 
									T; a null arglist unifier 
                                     ; implies (trivial) success
                                   	(if (member nil x) 
                                   		nil
                                   		x
                                   	)
                                 )
                     ); a null element indicates a failed substitution
                   	 (arglist-unifier (cdr lit1) (cdr lit2)) 
                   	)
				)
			)
		)
	)
)

(defun subst-unifier (uni wff);
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Substitute for variables of wff as specified by unifier uni.
; uni may be T (trivial unifier, for which we return wff) or of form
; ((var1 . term1) ... (vark . termk))
	(prog ((wff-out wff))
		(when (equal uni t) (return wff))
        (dolist (pair uni) 
			(setq wff-out (subst (cdr pair) (car pair) wff-out))
		)
		(return wff-out)
	)
)

(defun subst-unifier-in-wffs (uni wffs);
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	(mapcar #'(lambda (wff) (subst-unifier uni wff)) wffs)
)

(defun arglist-unifier (list1 list2)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Unify equal-length lists of (perhaps functional) terms where the
; terms in `list1' may be, or may contain, variables (Lisp symbols 
; with initial character `?'). Here result nil indicates trivial
; success, & (nil) indicates failure.
	(if (null list1) 
		nil
      	(if (equal (car list1) (car list2))
          	(arglist-unifier (cdr list1) (cdr list2))
          	(if (var (car list1))
              	(cons (cons (car list1) (car list2))
                      (arglist-unifier
                       	(subst (car list2) (car list1) (cdr list1))
                       	(subst (car list2) (car list1) (cdr list2))
                      )
                )
              	; initial complex terms?
              	(if (and (listp (car list1)) (listp (car list2))
                       	 (= (length (car list1)) (length (car list2)))
                    )
                  	(let ((uni (arglist-unifier (car list1) (car list2))))
                       	(append uni
                          	(arglist-unifier 
                             	(subst-unifier uni (cdr list1)) 
                             	(cdr list2) 
                             )
                        )
                    )
                 	'(nil) 
                )
			)
		) ; nil in unifier list signals failure
	)
); end of arglist-unifier



   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;                                                          ;;
   ;; ROUTINES FOR DEFINING AND INSTANTIATING ACTION OPERATORS ;;
   ;; ======================================================== ;;
   ;;                                                          ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct op      ; an operator (type of action), or instance of it
;~~~~~~~~~~~~~
    name            ; name of the action type (even for an instance)
                    ; whose value (under eval) is this operator;
    instance        ; name of this instance (or nil, for an operator),
                    ; whose value (under eval) is this operator instance;
    pars            ; variables, starting with "?" (e.g., ?x, ?y, ...),
                    ; or specific values (ground terms), for instances;
    preconds        ; a list of positive or negative literals, containing
                    ; parameters or constants as arguments; evaluable
                    ; predicates (<, <=, =, >=, >, or ones ending in "?")
                    ; and functional terms, including evaluable ones
                    ; (+, -, *, /, or ones ending in "?") are allowed
                    ; as well;
    effects         ; a list of positive or negative literals -- same
                    ; syntax as for preconds; again we allow evaluable
                    ; predicates and functional expressions, which can
                    ; be Lisp-eval'ed whenever they don't embed variables.
                    ; e.g.,
                    ;     (+ 2 (weight-of? ?obj)).
                    ;
                    ; After ?obj has been replaced by, say, Box3, this
                    ; would be automatically evaluated as
                    ;     (eval (+ 2 (weight-of? 'Box3)));
                    ; Note that in this case the user-supplied 'weight-of?'
                    ; Lisp function would have to handle a symbolic
                    ; argument (and quite possibly, access gridworld
                    ; knowledge in the evaluation);
    time-required   ; estimated time required, which should be numerical
                    ; or a lisp expression that can be evaluated if all
                    ; parameters therein (if any) are replaced by
                    ; specific values; (EVALUATION NOT IMPLEMENTED YET)
    value           ; the inherent reward (or cost) of the operator,
                    ; which could be numerical or a lisp expression
                    ; which can be evaluated if all parameters therein
                    ; (if any) are replaced by specific values;
); end of op


(defun instantiate-op (op uni); Revised 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Form an instance of action type 'op', returning a (generated)
; name of the instance, as a variant of the name of 'op'
; The procedure is formulated in such a way that it could be
; used equally well for partial instantiation as for full 
; instantiation of an operator.
;
	(when (null uni) (return-from instantiate-op nil))
	(let* ((name (op-name op))
           (instance (gensym (string name)))
           (pars (op-pars op))
           (preconds (op-preconds op))
           (effects (op-effects op))
           (time-required (op-time-required op))
           (value (op-value op))
          )           
		(when (not (eq uni t)); not the trivial unifier
        	(dolist (u uni)
           		(setq pars (subst (cdr u) (car u) pars)) 
           	)
;(format t "~% NAME ~a ~a ~%" name pars)   
        	(dolist (u uni)
           		(setq preconds (subst (cdr u) (car u) preconds)) 
           	)
        	(dolist (u uni)
           		(setq effects (subst (cdr u) (car u) effects)) 
           	)
;(format t "~% BEFORE EFFECTS ~A ~%" effects) 
        	(setq effects (mapcar #'simplify-value effects))
        	(when (evaluable-func (car effects))
        		(setq effects (simplify-value effects))
        	)
;(format t "~% AFTER EFFECTS ~A ~%" effects) 
        	(dolist (u uni)
           		(setq time-required (subst (cdr u) (car u) time-required)) 
           	)
;(format t "~% BEFORE TIME~%") 
        	(setq time-required (simplify-value time-required)) 
;(format t "~% AFTER TIME ~%") 
        	(dolist (u uni)
           		(setq value (subst (cdr u) (car u) value))
           	)
;(format t "~% BEFORE VALUE~%") 
        	(setq value (simplify-value value)) 
;(format t "~% AFTER VALUE ~A ~A~%" name value) 
        )
        
      	(set instance
           (make-op :name name
                    :instance instance
                    :pars pars
                    :preconds preconds
                    :effects effects
                    :time-required time-required
                    :value value )) 
      	instance ; return name of instance
	)
); end of instantiate-op


 (defun simplify-value (expr); Revised 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Evaluate/simplify a func/pred expression expr that may contain 
; (a) user-defined Lisp func/preds (ones with a name ending in "?"),
; (b) arithmetic functions  +,-,*,/, or relations <,<=,=,>=,>, or
; (c) ordinary logical func/preds (i.e., ones that are interpreted 
; without being Lisp-evaluated).
;
; "Evaluation" here really means simplification, and is only
; carried out to the extent that the arguments of the outermost
; func/pred are variable-free. For example, an expression (+ 1 1 ?x)
; is not further simplified, and (+ 1 (* 2 3) ?x) is simplified 
; just to (+ 1 6 ?x). The arguments of each func/pred in expr are
; simplified if possible, and if the func/pred is evaluable and
; its arguments are variable-free, it is procedurally applied 
; to its argument before a result is returned. If it is not
; evaluable, the expr is returned unchanged except for the
; simplification of its args (if possible).
;
; Note that an expression like (loc-of Robbie) would be returned
; unchanged, whereas (loc-of? Robbie) would be evaluated, with
; the argument in effect quoted. (The world model might be accessed
; for the evaluation.) An expression like (+ 1 cost) would cause
; an error, because an attempt would be made to apply the Lisp
; '+' function to arguments 1 and 'cost. (So if 'cost' is to be
; obtained in some implicit way, e.g., by consulting gridworld
; knowledge, then a new function such as '+?' or 'sum?' should
; be defined and used.)
;
;(format t "~% expr is ~A ~A ~%" expr (atom expr))
 (cond ((atom expr) expr)
       ((and (not (contains-var expr))
             (evaluable-func (car expr))) ; +,-,*,/,<,<=,=,>=,>,random, 
                                          ; or ends in "?"
                                          ;(prog2
                                          ;(format t "~% here ~A ~%" expr)
        	(apply (car expr) (mapcar #'simplify-value (cdr expr))))
((or (eq (car expr) 'answer_to_whq.actual?) 
     (eq (car expr) 'answer_to_whq?))       		
     (apply (car expr) (mapcar #'simplify-value (cdr expr))));(mapcar #'simplify-value (cdr expr))))
       (t (cons (car expr) (mapcar #'simplify-value (cdr expr))))
 )
)

(defun evaluable-func (f) ; must be symbol, & end in ? or be one 
                           ; of +,-,*,/,<,<=,=,>=,>,random
  (and (symbolp f) (or (member f '(+ - * / < <= = >= >))
  					   (eq f 'random)
                       (char= (car (last (coerce (string f) 'list))) #\?)))
)

(defun contains-var (expr)
;~~~~~~~~~~~~~~~~~~~~~~~~~~
; Does Lisp expression 'expr' contain a variable (atom starting
; with "?") at any structural level?
;
 (cond ((var expr) t)
       ((atom expr) nil)
       ((find-if #'contains-var expr) t)
       (t nil) ))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;                                                            ;;
   ;; ROUTINES FOR FORWARD CHAINING AND PLAN SELECTION/EXECUTION ;;
   ;; ========================================================== ;;
   ;;                                                            ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 (defstruct state-node    ; a node in a tree of states generated
                          ; by forward search;
     terms                ; a list of terms in the wffs in wff-htable        
     name                 ; a (generated) atomic name, whose value
                          ; (via eval) is this state-tree node
     wff-htable           ; a hashtable of ground atomic wffs defining
                          ; the state (though the ground requirement,
                          ; and prohibition of negations may
                          ; eventually be relaxed);
     children             ; a list ((action-name_1 . state-node-name_1) 
                          ; ... (action-name_k . state-node-name_k))
                          ; pairs, where each action-name_i is the name
                          ; of an action instance and state-node-name_i
                          ; is the name of the corresponding successor
                          ; state; some of these children might have no
                          ; successors at a given time, others might
                          ; have successors to various depths;
     operators            ; list of the names of the operators that
                          ; were used so far in generating children;
                          ; more  might yet be added, generating further
                          ; children;
     parent               ; the (action-name . state-node-name) pair
                          ; which generated this state. For the very
                          ; first state in Gridworld, this is nil;
     local-value          ; a numerical value for the "desirability"
                          ; (reward) of that state, presumably
                          ; computed by taking the initial state to
                          ; have 0 value, and then computing changes
                          ; in state-value based on the effects of
                          ; each action taken since that initial state
                          ; (these values are in general estimates,
                          ; because states are in general predicated
                          ; rather than real);
     forward-value        ; the estimated cumulative value of the best
                          ; plan starting at this state (not counting 
                          ; the local-value at the present state);
                          ; this counts both the inherent values of
                          ; the actions of the best plan and the states
                          ; generated by that plan. 
 ); end of state-node


(defun chain-forward (state-node search-beams)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Chain forward from the given state (hashtable of +ve ground wffs),
; conducting a beam search using the operators and beam widths
; specified in 'search-beams', and return the forest of plans,
; attached to 'state-node' with the best plan first (i.e., 
; following leftmost branches in the leftmost tree). The idea 
; is to call this program, and then actually execute the first 
; step of the best plan, and then iterate. If we stick to the
; same beam-width after executing the first step, and the beam-
; widths for successive steps are decreasing, then after each 
; step the forward search just adds some more branches to the
; plan-tree, rather than starting from scratch.
;
; The actions formed as children of the given state-node are
; provided as output (along with their values), but the "real"
; output consists of the changes made to the planning tree
; emanating from the given state-node (whose leftmost, i.e., 
; seemingly best, action sequence also becomes available as
; the value of *plan*, and whose corresponding leftmost state
; sequence becomes available as *states*, where a state is
; represented as a set of ground predications).
;
; state-node: a structured state node, from which we are
;        to chain forward (in general, adding to children that
;        are already present);
; search-beams: a list ((n_1 . ops_1) ... (n_k . ops_k)), where the
;        n_i are numerical upper bounds on the number of distinct
;        successor actions to be searched further from, when adding
;        the ith step of any plan obtained in the forward-chaining,
;        and each ops_i is a list of operators (specified by name)
;        to be considered (in addition to ones that may have been
;        considered in a previous iteration) when adding possible
;        ith steps to any plan.
;
 (if (null search-beams) (return-from chain-forward nil))
 (let* ((state-node-name (state-node-name state-node))
        (wff-htable (state-node-wff-htable state-node))
        (children (state-node-children state-node))
        (operators (state-node-operators state-node))
        (beam (car search-beams))
        (nbest (car beam)) ; a numerical beam width
        (ops (cdr beam)) ; a list of operator names
        (extra-ops (set-difference ops operators))
        action-state-pairs random-best (max-value 0) (index 1)
       )
   ; METHOD from this point on:
   ;
   ; if extra-ops is non-nil, then we need to generate all
   ;    children using them, adding them to children and evaluating
   ;    them, and merge-sorting them into the preexisting children;
   ; find the nbest children generated by the preexisting and newly
   ;    given ops, and recursively chain forward from each of them
   ;    using (cdr search-beams); 
   ; re-order the children in order of highest (forward-value
   ;    + action value + local-value of successor state), say = max;
   ; reset the children of the current state-node;
   ; reset the forward-value of state-node to max.
   ;
   ; Note that this correctly resets forward-values for all children
   ; generated by ops, and their successors (for the operators
   ; specified in 'search-beams');
   ;
   (setq *node-now* state-node)	

   (when extra-ops
      (setf (state-node-operators state-node)
            (append extra-ops operators) 
      )
      (setq action-state-pairs
         (apply #'append
             (mapcar 
                #'(lambda (o) 
                     (all-instances-of-operator o state-node-name))
                extra-ops
             )
         )
      )
      (setq action-state-pairs ; sort them
          (sort action-state-pairs #'> :key #'inclusive-value) 
      )
      (setq children ; merge new actions into them
          (merge 'list action-state-pairs children #'>
                 :key #'inclusive-value)
      )
   )
	
	
   ; Recursively chain forward from the nbest children -- i.e., 
   ; the ones earliest in the list:
   (setq action-state-pairs (first-n children nbest))
   (dolist (pair action-state-pairs); search forward recursively
      (chain-forward (eval (cdr pair)) (cdr search-beams)) 
   )
   ; reorder recursion-pairs, since they now have new back-
   ; propagated values:
   (setq action-state-pairs
       (sort action-state-pairs #'> :key #'inclusive-value) )
   ; Merge them back into 'children':
   (setq children ; merge new actions into them
       (merge 'list action-state-pairs (nthcdr nbest children)
               #'> :key #'inclusive-value ))

	(when children
	  (setq max-value (inclusive-value (car children)))	
	  (while (and (< index (length children)) (= max-value (inclusive-value (nth index children))))
		(setq index (incf index 1))
	  )
	  (when (> index 1)
		; Randomly (with equal probability) pick the best children.
	   	(setq random-best (nth (random index) children))
	   	(setq children (delete random-best children :test #'equal))
	   	(setq children (push random-best children))
	  )
	)

   ; reset the children of the current state-node:
   (setf (state-node-children state-node) children)
   ; reset forward-value of state-node (=> back-propagation)
   (when children
      (setf (state-node-forward-value state-node)
            (inclusive-value (car children)) ); seemingly best child
      ; reset *plan*
      (setq *plan* (leftmost-action-sequence (car children))) 
      (setq *states* (leftmost-state-sequence (car children)))
   )
   (show-actions-and-forward-values children) ; return actions with
         ; their parameters, and the inclusive-values of the actions.
 )
); end of chain-forward


(defun leftmost-action-sequence (action-state-pair)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Starting with the action in the given action-state-pair, 
; continue tracing  leftmost successors till a state with no
; children is reached. Return the sequence of actions.
;
; action-state-pair: a dotted pair consisting of the name of
;       a specific action and the name of a state-node
;
  (prog ((pair action-state-pair) action-name state-node-name
         plan children )
        (if (null pair) (return nil))
   next (setq action-name (car pair) state-node-name (cdr pair))
        ; as 'action-type' we use the operator name along with
        ; the values of the parameters
        (push (action-type action-name) plan)
        (setq children (state-node-children (eval state-node-name)))
        (if (null children) (return (reverse plan)))
        (setq pair (car children))
        (go next)
  )
); end of leftmost-action-sequence


(defun leftmost-state-sequence (action-state-pair)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Starting with the parent state of the given action-state-pair, 
; continue tracing leftmost successor states till a state 
; with no children is reached. Return the sequence of states.
;  
  (prog ((pair action-state-pair) 
         (parent (state-node-parent (eval (cdr action-state-pair))))
         action-name state-node-name states children )
        (if (null pair) (return nil))
        (if parent 
           (setq states 
             (list (state-node-wff-htable (eval (cdr parent))))
           )
        )
   next (setq action-name (car pair) state-node-name (cdr pair))
        (push (state-node-wff-htable (eval state-node-name)) states)
        (setq children (state-node-children (eval state-node-name)))
        (if (null children) (return (reverse states)))
        (setq pair (car children))
        (go next)
  )
); end of leftmost-state-sequence

     
(defun action-type (action-name)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Show the name of the operator together with its parameters
;
  (cons (op-name (eval action-name))
        (op-pars (eval action-name)) ))


 (defun show-actions-and-forward-values (action-state-pairs)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Show each action type & the corresponding arguments, postfixed
; with the inclusive value (the value of the action, plus the
; local-value of the state, plus its forward-value)
;
  (mapcar #'(lambda (pair) 
              (cons (action-type (car pair))
                    (inclusive-value pair) ))
           action-state-pairs ))
      

 (defun inclusive-value (action-state-pair)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 'action-state-pair' is of form (action-name . state-node-name).
; Its 'inclusive-value' is the sum of: the value of the action
; named by 'action-name', the local-value of the state implicit
; in 'state-node-name', and the 'forward-value' of that state.
	(let ((num1 'NIL) num2 num3)
		(setq num1 (op-value (eval (car action-state-pair))))
		(when (not (numberp num1))
			(setq num1 0)
		)
		(setq num2 (state-node-local-value (eval (cdr action-state-pair))))
		(setq num3 (state-node-forward-value (eval (cdr action-state-pair)))) 
;(format t "~%IV action ~A currstate ~A  ~A ~A ~A ~A~%" (op-name (eval (car action-state-pair))) (state-node-name (eval (car action-state-pair))) 
;(+ num1 num2 num3) num1 num2 num3)
		(+ num1 num2 num3)	
	)

)


(defun all-instances-of-operator (op-name state-node-name); Revised Dec.'09 by D. Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Return all instances of the operator named by 'op-name',
; paired with the names of the resulting state-nodes, that can
; be formed by binding the preconds of the operator to the wffs
; in the state with name 'state-node-name'. So the output list
; consists of pairs (action-name . state-node-name), where the
; values of these (new) names are an operator instance and a
; state-node respectively. Use 'all-bindings-of-goals-to-fact-htable',
; and 'instantiate-op'.
;
	(let* ((op (eval op-name))
           (state-node (eval state-node-name))
           (preconds (op-preconds op))
           (wff-terms (state-node-terms state-node))
           (wff-htable (state-node-wff-htable state-node))
           bindings
           instances
           state-nodes 
          )  
          
		(setq bindings 
             (all-bindings-of-goals-to-fact-htable 
             		preconds wff-htable wff-terms)
       	)
       	
       	; Filter out bindings that contain the same values
       	; for different variables.
       	(setq bindings
             (remove-duplicates (remove-if 
             		#'degenerate-binding bindings) :test #'equal)
        )
        
       	; Generate an instance for each binding.
       	(setq instances
             (mapcar #'(lambda (u) (instantiate-op op u)) bindings)
        ) 
                      
       	; This list of instance names now needs to be augmented
       	; with names of resulting states; for this we need to
       	; apply the effects of each operator instance to the
       	; wff-htable of state-node:
       	   
       	(setq state-nodes 
             (mapcar 
                #'(lambda (i) 
                    (generate-state-node i state-node-name) ) 
                instances)
        )
       	(mapcar #'cons instances state-nodes); return pairs
	)
); end of all-instances-of-operator


(defun degenerate-binding (u)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; A unifier that assigns the same value to different variables;
; We assume that such bindings are not allowed for the operators
; under consideration. For example, transferring an object ?x from
; ?y to ?z only makes sense when ?x, ?y, ?z are all distinct.
;
	(if (atom u) 
		nil 
      	(> (length u)
         	(length (remove-duplicates (mapcar #'cdr u) 
                                    :test #'equal))
        )
	)
)


(defun generate-state-node (action-name state-node-name); Revised Dec. 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Generate a named state-node as child of the given state-node,
; generated by applying the effects of the given action to
; the given state.
;
  (let* ((action (eval action-name))
         (effects (op-effects action))
         (deletions (neglits effects))
         (additions (poslits effects))
         (state-node (eval state-node-name))
         (new-terms (state-node-terms state-node))
         (old-wff-htable (state-node-wff-htable state-node))
         (local-value (state-node-local-value state-node))
         (new-state-node-name (gensym "STATE-NODE"))
         new-local-value
         new-forward-value
         (new-wff-htable (copy_construct_hashtable old-wff-htable))
        )
        ; Remove deletions that are the same as additions:
        (setq deletions (set-differencef deletions additions))
        ; Remove additions that are the same as existing old-wff-htable:
        ;(setq additions (set-differencef additions old-wff-htable))
        ; Thus only real changes are counted in 'new-local-value':
        (setq new-local-value ; inherent value of the new state
              (state-value additions deletions local-value))
        (setq new-forward-value      ; expected future rewards/costs, for 
              (expected-rewards old-wff-htable)); action sequences (& their effects) 
                                ; starting at the new state; this seems
                                ; like a candidate for learning, based
                                ; on experience in starting from similar
                                ; states; but we'll have to settle for 
                                ; some sort of heuristic guess for now --
                                ; maybe the same value for all states 
                                ; (with an "optimistic" bias); later this
                                ; value gets replaced by back-propagated
                                ; values, based on action sequences
                                ; generated from the new state;
        ; Update the set of old-wff-htable to reflect the new state.
	
		(setq new-terms (removef (remove_list_of_tuples_from_hashtable 
									deletions new-wff-htable 'T) new-terms))
		(setq new-terms (append (add_list_of_tuples_to_hashtable additions new-wff-htable 'T)
							new-terms))

        ;(setq new-wff-htable (append additions (set-differencef new-wff-htable deletions)))
        ; ** At this point we *should* insert forward inferencing (in case
        ; ** there are inference rules associated with dynamic properties)
        ; ** This would use `all-inferences', as in `initialize-state-node'.
        ; ** We would have to distinguish (actual) *protected-facts* from
        ; ** presumed ones in a new 'protected-facts' field in state-nodes.
        ; ** However, because inferencing is rather inefficient at present
        ; ** we omit it for now, doing it only in "actual" new states.
        (set new-state-node-name
             (make-state-node
             	:terms new-terms
                :name new-state-node-name
                :wff-htable new-wff-htable
                :children nil
                :operators nil
                :parent (cons action-name state-node-name)
                :local-value new-local-value
                :forward-value new-forward-value ))
         ;(state-node-new-wff-htable (eval new-state-node-name)); debugging
        new-state-node-name
 )
); end of generate-state-node


(defun name-of-actual-operator (name.actual-str state-node); Revised in April 2008
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Create an (actual) operator whose name is obtained by interning
; name.actual-str and whose value is a copy of state-node, except
; for adjustment of the op-name field to contain the interned name
; (and maybe set to nil the op-value field, since the value is relevant
; only to the subjective version of the operator? Or is this pointless?)
;
  (let ((name.actual (intern name.actual-str)))
       (set name.actual (copy-state-node state-node))
       (setf (op-name (eval name.actual)) name.actual)
       name.actual 
  )
)

 
(defun initialize-state-node ( ); Revised Dec. 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; This augments the world state (*world-facts*) with inferences
; from *general-facts*, and then sets up AG's initial state-node,
; creating a new state-node name and providing its value via
; `make-state-node'. This value also becomes *curr-state-node*.
; Most importantly, it sets the `wffs' field of that state-node
; so as to cover the *roadmap-knowledge* (we should perhaps consider
; whether to just use facts about nearby roads/points?), plus the
; "non-occluded" facts (that should be evident to the AG agent)
; about entities located at *here* (the point at which the AG agent
; has been placed by the user). The `local-value' of the initial 
; state is set to 0 -- this is an arbitrary reference value, and
; the `forward-value' is set by default to, say, 2 (larger values
; would mean greater "optimism about what the future may bring"). 
;
	(let	(local-facts local-facts-terms-triple implied-facts
          	 (new-state-node-name (gensym "STATE-NODE"))
          	 (location (find-location 'AG *world-facts*))
       		)
       
		; The assumption is that the user would not call function 
		; initialize-state-node to re-initialize, but only at 
		; the onset of execution for initialization purpose only. 
		   
		; Check for predefined roadmap knowledge.
       	(when (null *roadmap-knowledge*)
           (return-from initialize-state-node
             "** You need to do a def-roadmap before initializing"))
       	; Check for presence of AG *here*.
       	(when (null location)
           (return-from initialize-state-node
             "** You need to do a place-object for AG before initializing"))      

       (setq *visited-places* (list location))
	   (setq *visited-objects* nil) ; updated in facts-evident-to function
	   
       ; Work out all the consequences in the world derivable from
       ; specific facts by applying general rules, & add to *world-facts*.
       ; Derive consequences at most *inference-limit* levels of iteration.
       ; At this point, *world-facts* and *protected-facts* are identical.
       (setq implied-facts
             (all-inferences *world-facts* *general-knowledge*
                                              *inference-limit*) 
       )
       (add_htable_to_hashtable implied-facts *world-facts* 'NIL)
       ; Now *world-facts* may be larger than *protected-facts*.
       ; Pick out the local world facts apparent to the agent.
       (setq local-facts-terms-triple (facts-evident-to 'AG *world-facts* 'T))
       
       (setq local-facts (first local-facts-terms-triple))
       (setq new-terms (second local-facts-terms-triple))
       ; Add any extra initial knowledge of the agent.
       (setq new-terms (append new-terms 
       		(add_htable_to_hashtable *extra-initial-knowledge* local-facts 'T)))
       (setq new-terms (append new-terms 
			(add_htable_to_hashtable *roadmap-knowledge* local-facts 'T)))
			
	(setq new-terms (append new-terms
		(add_htable_to_hashtable (all-inferences local-facts *general-knowledge* *inference-limit*) local-facts 'T)))
		    
       ; Create a corresponding initial state node for AG.
       (set new-state-node-name
            (make-state-node
               :terms new-terms
               :name new-state-node-name
               ; We include the roadmap knowledge in the agent's
               ; current knowledge of the world.
               :wff-htable local-facts
               :local-value 0
               :forward-value 2 )); NB: :parent is nil by default
       (setq *event-queue* '())
       (setq *real-clock* 0)
       (setq *AG-clock* 0)
       (setq *total-value* 0)
       (setq *real-history* '())
       (setq *AG-history* '())
       (setq *curr-state-node* (eval new-state-node-name))
       (setq *node-now* *curr-state-node*)
 )
); end of initialize-state-node

(defun facts-evident-to (agent wff-htable to-collect-terms); Revised Dec. 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Method:
; Find objects colocated with agent (according to wff-htable);
; then return a hashtable of all wffs in wff-htable that 
; have objects colocated with the agent in subject position, 
; and are not occluded to agent, as well as terms occurring 
; in such formulas.
;
; # Note that a fact is not considered occluded if its first
; argument is AG, even if the predicate is occluded.
;
  (let* ((objects (objects-colocated-with agent wff-htable))
  		(aug-objects (cons agent objects))
        (evident-wff-htable (make-hash-table :test #'equal))
        wffs-at-curr-key
        terms-evident-to
        list-of-evident-wffs
       )
       (loop for value being the hash-values of wff-htable
        	using (hash-key key)
	        do	(setq wffs-at-curr-key 
	        		(remove-if #'occluded-fact
	     					(remove-if-not 
	        					#'(lambda (w) (memberf (second w) aug-objects)) (cdr value)
	        				)
	        		)
	        	)    	
	        	(setq terms-evident-to (append 
	        		(add_list_of_tuples_to_hashtable
	        			wffs-at-curr-key evident-wff-htable to-collect-terms)
	        		terms-evident-to)
	        	)
	        	(setq list-of-evident-wffs (unionf wffs-at-curr-key list-of-evident-wffs))
  		)
  		
  		(cons evident-wff-htable (list terms-evident-to list-of-evident-wffs))
  )
); end of facts-evident-to
     

(defun objects-colocated-with (agent wff-htable); Revised Dec. 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Find objects colocated with agent (according to wff-htable) as follows.
; - find agent's location loc (from 'is_at' fact about it);
; - local-objects := those x's such that (is_at x loc) is in wff-htable;
; - select left-comoving-wffs and right-comoving-wffs from wff-htable;
; - keep iterating through these two lists of wffs, checking
;   for each one whether there's a new object comoving with one
;   already on the local-objects list; add those found to that
;   local-objects list; when now more new comoving objects are
;   found, return local-objects;
;
;   As examples of the relevant wffs, (has u v) is right-comoving
;   and (is_in u v) is left-comoving; gridworld-definitions.lisp
;   is assumed to supply the lists *left-comoving-preds* and 
;   *right-comoving-preds*;
;
  (prog (loc key local-objects left-comoving-wffs right-comoving-wffs
        something-new )
      
       ; Find location of agent
       (setq loc (find-location agent wff-htable)) 
       
       ; Find explicitly colocated objects
       (setq key (convert_pred_to_hashkey (list 'is_at '?x loc)))
       (setq local-objects
          (mapcar #'second (cdr (gethash key wff-htable))))
          
       ; Find implicitly colocated objects;
       ; Form list of wffs involving comoving predicates:
       (setq left-comoving-wffs
          (apply 'append    
             (mapcar #'(lambda (x) (cdr x))  
               (mapcar #'(lambda (key) (gethash (list key) wff-htable)) *left-comoving-preds*)
             ) 
          )
       )
              
       (setq right-comoving-wffs
          (apply 'append    
             (mapcar #'(lambda (x) (cdr x))  
               (mapcar #'(lambda (key) (gethash (list key) wff-htable)) *right-comoving-preds*)
             ) 
          )
       )
              
       ; keep checking for new comoving objects as long as new
       ; ones are found; (no. of iterations will be at most the
       ; length of the longest comoving dependency chain):
  more (setq something-new nil)
       (dolist (wff left-comoving-wffs)
          (when (memberf (third wff) local-objects)
                (setq something-new nil)
                (push (second wff) local-objects) ))
       (dolist (wff right-comoving-wffs)
          (when (memberf (second wff) local-objects)
                (setq something-new nil)
                (push (third wff) local-objects) ))
       (if something-new (go more))
       
       (setq local-objects (remove-duplicates local-objects :test #'equal))
       (setq *visited-objects* (unionf *visited-objects* local-objects))
       
       (return local-objects)
 )
); end of objects-colocated-with


(defun occluded-fact (wff); Revised in April 2008
;~~~~~~~~~~~~~~~~~~~~~~~~~~
; Return non-nil result if the predicate of the wff is occluded
; and the first ("subject") argument is not `AG'. Examples of
; occluded facts might be (hungry Grunt) (but not (hungry AG)), 
; (is_hidden_in Key2 Box3), or (knows_that Grunt (has AG banana3)).
;
  (cond ((atom wff) (member wff *occluded-preds*)); unexpectd
        (t (and (member (car wff) *occluded-preds*)
                (or (null (second wff)) 
                    (not (eq (second wff) 'AG)) )))
  )
)


(defun all-inferences (ground-facts-htable general-facts limit)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~; Revised Dec.'09, D.Liu
; Make all inferences enabled by the `general-facts' based on
; the `ground-facts-htable'. Iterate, up to `limit' times, for any new
; inferences.
;
; ground-facts-htable: ground atomic wffs;
; general-facts: Horn wffs of form (phi => psi), i.e., phi
;    is either an atomic wff (possibly containing variables) or
;    a conjunction (and phi_1 ... phi_k) where the phi_i are
;    atomic, and psi is atomic.
; METHOD: 
;  {We refer to the wffs in `general-facts' as "rules" here,
;  for clarity of exposition; 'results' is initially nil}
;  1. Let rule-packets := a list where each element is of form
;     (rule), where `rule' is one of the rules (general-facts);
;  2. Let facts-htable : = ground-facts-htable;
;  3. Repeat steps a-b at most `limit' times, stopping if facts
;     = nil:
;     a. Let (facts-htable . rule-packets) :=
;                 new-inferences(facts-htable, rule-packets);
;        {This gives (i) the new inferences derivable in one
;        step (one full rule instantiation of a rule in some
;        rule packet) from the `facts' along with facts
;        included in the rule packets (initially none), and 
;        (ii) new rule-packets in which wffs from the given
;        `facts' that match the rule packet have been added to
;        each packet.}
;     b. results := append(facts-htable, results);
;  4. Return `results'.
;  
  
  (let ((rule-packets (mapcar #'list general-facts))
        (facts-htable ground-facts-htable)
        facts-and-packets
        (inferences-htable (make-hash-table :test #'equal))     
       )
       (dotimes (i limit)
         (when (zerop (hash-table-count facts-htable))
             (return-from all-inferences inferences-htable)
         )
         (setq facts-and-packets
            (new-inferences facts-htable rule-packets inferences-htable))
         (setq facts-htable (car facts-and-packets)
               rule-packets (cdr facts-and-packets))
       )
       inferences-htable
 )
); end of all-inferences


(defun new-inferences (facts-htable rule-packets inferences-htable)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~; Revised Dec.'09, D. Liu
; Generate new inferences, and new rule packets, based on the
; given (unprocessed) `facts-htable' and the rule plus facts to be
; found in each rule packet. Generate only 1-step inferences,
; i.e., obtainable by matching all antecdents of a rule,
; without feeding new consequences back into the facts-htable. 
; Return the list of inferences cons'd into the list of 
; rule packets (the latter augmented with antecedent-matching 
; facts from those in the input).
; 
; facts-htable: ground atomic wffs;
; rule-packets: a list with elements of form (rule fact_1 ...
;    fact_n) where each rule is of form (phi => psi), with phi 
;    either an atomic wff (in general, with variables) or of
;    form (and phi_1 ... phi_k), where ph_i and psi are atomic
;    wffs. Facts are ground atomic wffs.
;
; METHOD:
; 1. Let double-packets := a list with each element of form
;    (rule ( ) f_1 ... f_k) where (rule f_1 ... f_k) was one
;    of the elements of `rule-packets';
; 2. For each fact in `facts-htable', and each double-packet, if the
;    fact unifies with some antecedent literal of the rule in the
;    double packet, then graft the fact into the list occupying 
;    second position (i.e., right after the rule) in the double
;    packet, provided that it isn't in that list, or in the cddr
;    of the double packet yet (use function `graft-into-packet');
; 3. For each double-packet with a non-nil 2nd element:
;    a. Find all 1-step inferences (based on full instantiation
;       of the variables in the rule) that use at least one
;       fact from the list of facts which is the 2nd element
;       of the double packet, and any number of pre-existing
;       facts in the packet (use the function `shake-packet');
;    b. Concatenate these inferences with `inferences';
; 4. For each packet (rule (g_1 ... g_m) f_1 ... f_k) form
;    (rule g_1 ... g_m f_1 ... f_k); call the result 
;    new-packets;
; 5 Return (cons inferences new-packets).
;
  (let ((double-packets
          (mapcar 
            #'(lambda (x) (cons (car x) (cons nil (cdr x))))
            rule-packets ))
        new-packets
       )
       (dolist (rp double-packets)
          (setq double-packets
             (mapcar #'(lambda (p) (graft-into-packet facts-htable p)) double-packets)
          )
       )

       (dolist (augm-packet double-packets)
          (when (second augm-packet)
             (shake-packet augm-packet inferences-htable)
		  )
       )

       (setq new-packets
           (mapcar #'(lambda (x) (append (list (car x)) (second x) (cddr x)))
             double-packets))
       (cons inferences-htable new-packets)
  )
); new-inferences


(defun graft-into-packet (facts-htable augm-packet)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~; Revised Dec. 2009 by Daphne Liu
; `wff' is a ground atomic formula;
; `augm-packet' is of form (rule (g_1 ... g_m) f_1 ... f_n)
; (for further details see `new-inferences' or `shake-packet'.)
;
; If wff does not appear among g_1 ... g_m or f_1 ... f_n, 
; then if wff unifies with some antecedent literal of the rule
; in the augm-packet, then graft the fact into the list occupying
; second position (i.e., right after the rule) in the double
; packet, provided that it isn't in that list, or in the cddr
; of the double packet yet; return the (possibly) altered
; packet;
;
(let* ((result augm-packet)
	   (rule (car result))
       (goals (goals-of-rule rule)) 
       (wff-list (cdr (gethash (convert_pred_to_hashkey (car rule)) facts-htable))) 
      ) 
    (dolist (wff wff-list)
        (when (and (not (memberf wff (second result)))
                 (not (memberf wff (cddr result)))
                 (find-if #'(lambda (g) (unifier g wff)) goals) )
            (setq result (cons rule (cons (cons wff 
            		(second result)) (cddr result))))
        )
    )
    result
)
); end of graft-into-packet


 (defun goals-of-rule (rule)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Return the goal-list (antecedents) of `rule' (see e.g., 
; `shake-packet' for forms of rules -- essentially Horn clauses,
; written as (antecedent => consequent)). Allow for either single-
; literal or AND'ed antecedent, and allow a literal in principle
; to be an atom like `HUNGRY or a literal with 0 arguments, such
; as ; (HUNGRY), even though these are advised against. We also
; allow omission of AND for an AND'ed antecedent, though this
; is unexpected.
;
  (let ((goals (car rule)))
       (cond ((null goals) nil); unexpected
             ((atom goals) (list goals)); unexpected
             ((eq (car goals) 'and) (cdr goals))
             ((listp (car goals)) goals); missing `and'
             (t (list goals)) ); single goal
 )); end of goals-of-rule

 
(defun shake-packet (augm-packet inferences-htable)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~; Revised Dec. 2009 by Daphne Liu
; `augm-packet' is of form (rule (g_1 ... g_m) f_1 ... f_n),
; where rule is form (phi => psi), with phi either an atomic wff 
; (in general, with variables) or of form (and phi_1 ... phi_k), 
; where ph_i and psi are atomic wffs; and the g_i and f_j are
; atomic ground wffs.
;
; Find all 1-step inferences (based on full instantiation of
; the variables in the rule) that use at least one fact from 
; the list (g_1 ... g_m), and any number of facts from the
; list (f_1 ... f_n).
;
; Method:
; 1. rule := first(augm-packet);
;    new-facts := second(augm-packet);
;    old-facts := cddr(augm-packet);
;    all-facts := append(new-facts,old-facts)
; 2. if new-facts =/= nil, then
;    a. goals := antecedents of `rule';
;    b. find all bindings of each goal in `goals' to new-facts,
;       and let `bindings' := the concatenation of the sets
;       of bindings found;
;    c. apply each binding u in `bindings' to `goals',
;       remove the resulting (variable-free) goals
;       that coincide with some facts in new-facts, find
;       bindings2 := all bindings of the remaining goals
;       (if any) to all-facts, and combine the binding u
;       with each of the ones in bindings2; apply the   
;       resultant bindings to the rule consequent, and 
;       push the resulting wff onto `inferences'
; 3. Return `inferences'.
;
  (let* ((rule (car augm-packet))
         (new-facts (second augm-packet))
         (old-facts (cddr augm-packet))
         (all-facts (append new-facts old-facts))
	 (new-facts-htable (make-htable new-facts))
         goals 
         goals2
         bindings
         bindings2
        )
       (setq goals (goals-of-rule rule))
       (setq bindings
         (apply #'append 
           (mapcar 
             #'(lambda (g)
                 (all-bindings-of-posgoal-to-fact-htable g new-facts-htable) )
              goals )))
       (dolist (u bindings)
          (setq goals2 (subst-unifier-in-wffs u goals))
          (setq goals2 (set-differencef goals2 new-facts))
          (setq bindings2
             (find-all-positive-bindings goals2 all-facts))
          (setq bindings2 
             (mapcar #'(lambda (u2) (*append u u2)) bindings2))
          (dolist (u2 bindings2)
             (add_tuple_to_hashtable (subst-unifier u2 (third rule)) inferences-htable 'NIL) 
          )
       )
       inferences-htable
  )
); end of shake-packet
          

(defun notice-new-local-facts (state-node); Revised Dec. 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Update the agent's beliefs in light of the locally evident
; facts (and beliefs that have evidently become false)
; - wff-htable := agent's beliefs at the given state-node
; - compute local-beliefs := (facts-evident-to 'AG wff-htable) 
; - delete from wff-htable those local-beliefs that are not local-facts
; - Compute local-facts := (facts-evident-to 'AG *world-facts*);
; - augment wff-htable (which are beliefs) with any unknown local-facts
; - add inferences based on updated wffs and *general-knowledge*
; - hence update agent's belief state
; - return the wff-htable comprising that belief state;
; *here* is also updated, just in case it's needed;
;
; After updating the wff-htable in the given state-node, return that 
; updated wff-htable.
;
; NOTE: This update strategy allows for (partial) ignorance of local
; facts, as well as ignorance about the effects of actions taken.
; The latter is true because we have both conceptual and actual
; versions of each each operator, and updating of beliefs is based
; on how the world actually changed (as far as these changes are
; locally observable.
;
  (let* ((wff-htable (state-node-wff-htable state-node))
  		 (new-terms (state-node-terms state-node))
         ; beliefs that *should* be evident facts if true:
         (local-beliefs (facts-evident-to 'AG wff-htable 'NIL))
         ; actual local unoccluded facts:
         (local-facts (facts-evident-to 'AG *world-facts* 'NIL))
         falsified-facts implied-facts
        )                 
        ; Now do belief updates
        (setq falsified-facts 
           (set-differencef (third local-beliefs) (third local-facts))
        )
        ; get rid of what is evidently no longer true:
        (setq new-terms (removef
        	(remove_list_of_tuples_from_hashtable falsified-facts wff-htable 'T)
        	new-terms)
        )
        ; add evident facts missing from the beliefs (if any):
        (setq new-terms (append 
        	(add_list_of_tuples_to_hashtable (third local-facts) wff-htable 'T)
        	new-terms)
        )
        
        ; add implied facts; most of these will already have been
        ; obtained via 'facts-evident-to', but it is possible that
        ; some newly observed local facts have nonlocal consequences,
        ; which the agent can infer:
        (setq implied-facts
           (all-inferences wff-htable *general-knowledge* *inference-limit*))
        (setq new-terms (append 
        	(add_htable_to_hashtable implied-facts wff-htable 'T)
        	new-terms)
        )
        ; reset wffs-field of state-node to the revised set
        ;(setf (state-node-wff-htable state-node) wff-htable)
        (setf (state-node-terms state-node) new-terms)
        (setq *here* (find-location 'AG wff-htable)); not sure if needed...
        
        (setq *visited-places* (remove-duplicates (cons *here* *visited-places*)))
        ; *visited-objects* updated in facts-evident-to function
        
        wff-htable ; return the (possibly) expanded set of wffs from the
             ; updated state-node.
  )
); end of notice-new-local-facts


(defun find-location (obj wffs-htable); Revised Dec. 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; find the first wff in `wffs' of form (is_at obj x), and return x
; (or nil, if there is no such wff). `obj' is a ground term, and `wffs'
; are positive ground predications.
;
;  (third (find-if #'(lambda (w) (and (listp w) 
;                                       (eq (car w) 'is_at)
;                                       (equal (second w) obj) ))
;                           wffs ))
	(let ((key (convert_pred_to_hashkey (list 'is_at obj '?x))) result)
                           
		(when (gethash key wffs-htable)
	  		(setq result (third (second (gethash key wffs-htable))))
		)
		result
	)
)

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;                                           ;;
   ;; TO BE REPLACED BY USER-SUPPLIED FUNCTIONS ;;
   ;;                                           ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun state-value (additions deletions prior-local-value)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~; Revised Dec.'09, D. Liu
; Allocate +ve points for addition of desirable properties, and
; subtract away that number of points for deletions of those
; properties; analogously for addition of undesirable properties;
; the additions and subtractions are made using 'prior-local-value'
; as starting value;
;
; Just as an example, let's suppose the agent AG likes knowing
; things, likes being liked, and likes having things, and
; dislikes being hungry. So we reward addition of wffs of form
; (know-whether AG ...), (know-that AG ...), (likes ... AG),
; (has AG ...); and we reward removal of (hungry AG). Also
; we punish (correspondingly) removal of (likes ... AG) and
; (has AG ...), and addition of (hungry AG). 
; 
  (let ((local-value prior-local-value) pred incr)
       (dolist (wff additions)
          (when (listp wff)
             (setq pred (car wff))
             (setq incr 
                (case pred
                   ((knows has) ; (know-whether know-that has) 
                    (if (eq (second wff) 'AG) 1 0) )
                   (wants 
                    (if (eq (second wff) 'USER)
                    	(if (listp (third wff)) 
                            (if (> (length (third wff)) 1)
                    		(if (listp (second (third wff)))
                    		    (if (> (length (second (third wff))) 3)
                    			(if (and (eq (first (second (third wff))) 
                                                     'tells)
			                    					 	                                               (eq (second (second (third wff))) 'AG)
			                    					 	                                               (eq (third (second (third wff))) 'USER))
			                    				-20 0
			                    		)
			                    		0
			                    	)
                    				0
                    			)
                    			0
                    		)
                    		0
                    	)
                    	0
                     )
                   )
                   (likes 
                    (if (eq (third wff) 'AG) 1 0) )
                   (bored
                   	(if (eq (second wff) 'AG) -1 0) )
                   (is_thirsty_to_degree 
                    (if (eq (second wff) 'AG) (- (third wff)) 0) )
                   (is_hungry_to_degree 
                    (if (eq (second wff) 'AG) (- (third wff)) 0) )
                   (is_tired_to_degree
                    (if (eq (second wff) 'AG) (- (third wff)) 0) )
                   (t 0) ))
             (incf local-value incr) ))
       (dolist (wff deletions)
          (when (listp wff)
             (setq pred (car wff))
             (setq incr 
                (case pred
                   (wants 
                    (if (eq (second wff) 'USER)
                    	(if (listp (third wff)) 
                    	    (if (> (length (third wff)) 1)
                    		(if (listp (second (third wff)))
                    		    (if (> (length (second (third wff))) 3)
                    			(if (and (eq (first (second (third wff))) 'tells)
			                    					 	                                               (eq (second (second (third wff))) 'AG)
			                    					 	                                               (eq (third (second (third wff))) 'USER))
			                    				50 0
			                    		)
			                    		0
			                    	)
                    				0
                    			)
                    			0
                    		)
                    		0
                    	)
                    	0
                     )
                   )
                   (likes
                    (if (eq (third wff) 'AG) -1 0) )
                   (bored
                   	(if (eq (second wff) 'AG) 1 0) )
                   (is_thirsty_to_degree
                    (if (eq (second wff) 'AG) (third wff) 0) )
                   (is_hungry_to_degree
                    (if (eq (second wff) 'AG) (third wff) 0) )
                   (is_tired_to_degree
                    (if (eq (second wff) 'AG) (third wff) 0) )
                   (t 0) ))
             (incf local-value incr) )) 
       local-value
  )
); end of state-value

     
(defun expected-rewards (wff-htable) ;(wffs)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; For the time being, this is a stub, returning a fixed value.
;
  2
)

