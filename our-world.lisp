;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Name: gridworld-world.lisp
;; This is the sample gridworld you can experiment with to familiarize 
;; yourself with the framework. Note that when creating your new gridworld,
;; you should modify the functions state-value and expected-value in
;; gridworld-planning.lisp to reflect the likes and dislikes of your agent.
;; 
;; Author: Daphne Liu
;; Date: Jan. 2010 by Daphne Liu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-roadmap '(home grove plaza) '((path1 home 3 grove) (path2 home 2 plaza)))
(def-object 'robot '(is_animate can_talk))
(def-object 'expert '(is_animate can_talk))
(def-object 'instrument '(is_inanimate is_playable))
(def-object 'juice '(is_inanimate is_potable (has_cost 2.0)))
(def-object 'pizza '(is_inanimate is_edible (has_cost 5.0)))

;; Note that we create some "current facts" about
;; AG that are really about the situation at plaza;
;; this is just a way of ensuring that AG knows these
;; facts right at the outset.
(place-object 'AG 'robot 'home 0  
  nil ; no associated-things
  ; current facts
  '((is_hungry_to_degree AG 4.0)
	(is_thirsty_to_degree AG 2.0)
    (is_tired_to_degree AG 0.0)
    (can_talk guru)
    (is_at guru grove)
    (is_at juice3 plaza)  
     ;Note that right after the call to function initialize-state-node, 
     ;AG knows (is_edible pizza3) and (is_playable piano2). The reason is
     ;AG knows the types of pizza3 and piano2 colocated with AG at home,
     ;and AG does forward inference using its initial knowledge in 
     ;conjunction with *general-knowledge* to derive the knowledge 
     ;of the type-specific properties of pizza3 and piano2. All this 
     ;occurs despite the general occlusiveness of the predicates 
     ;is_edible and is_playable as specified by *occluded-preds*.
     ;This behavior is acceptable. To prevent AG from knowing about 
     ;the edibility of pizza3 and playability of piano2 at the outset,
     ;one would need to specify general inference rules from AG to use,
     ;separate from *general-knowledge*. But for simplicity, we specify
     ;only *general-knowledge* as the inference rules both known in the 
     ;simulated world and used by AG.
  ) 
  ; propositional attitudes
  '((knows AG (whether (is_playable piano2)))
    (knows AG (whether (is_edible pizza3)))
    (knows AG (that (knows guru (whether (is_potable juice3))))) 
    ;merely (knows guru (whether (is_potable juice3))) won't work, because (knows guru ...) is first
    ;deposited into *protected-facts* and *world-facts* via place-object, and then later filtered 
    ;to see if it should be known (added to local facts) to AG in initialize-state-node. And 
    ;guru's knowledge is occluded and so filtered out. So the bug fix for now is that when you want 
    ;to initially assign to AG the knowledge of some other agent's knowledge, you should prefix 
    ;that with `knows AG that', and hence the form (knows AG (that (knows another_agent ...))).
   )
)

(place-object 'pizza3 'pizza 'home 0 
	nil ; no associated-things
	; current facts
	'((is_edible pizza3) 
	 )
    nil ; propositional attitudes
)

(place-object 'juice3 'juice 'plaza 0 
	nil ; no associated-things
	; current facts
	'((is_potable juice3) 
	 )
    nil ; propositional attitudes
)

(place-object 'piano2 'instrument 'home 0 
	nil ; no associated-things
	'((is_playable piano2)
	 )
    nil ; propositional attitudes
)

(place-object 'guru 'expert 'grove 0 
	nil ; no associated-things
    nil ; no current facts
    ; propositional attitudes
    '((knows guru (whether (is_potable juice3)))
     )
)

;(setq *occluded-preds* 
;    '(is_playable knows is_edible is_potable)
; We omit this, as *occluded-preds* is currently already set in 
; "gridworld-definitions.lisp".

(setq *operators* '(walk eat answer_user_ynq answer_user_whq sleep drink ask+whether play))
(setq *search-beam*
;(list (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) ))
	(list (cons 5 *operators*) (cons 4 *operators*) (cons 3 *operators*) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator fire.actual is the exogenous fire operator.  As long as there 
;; is no rain, a spontaneous fire has a 5% chance of starting; once 
;; it has started, it has a 50% chance of stopping, and it also goes out 
;; as soon as there is rain.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq fire.actual 
	(make-op.actual :name 'fire.actual :pars '()
    :startconds '((not (there_is_rain))
				  (= 3 (random 20))) ; 5% chance of fire starting
    :starredStopConds '((= 1 (random 2)) ; 50% chance of stopping after starting
						(there_is_rain))
    :starredDeletes '((there_is_a_fire))
    :starredAdds '((navigable PATH1) (navigable PATH2) (navigable PATH3) (navigable PATH4))
    :deletes '((navigable PATH1) (navigable PATH2) (navigable PATH3) (navigable PATH4))
    :adds '((there_is_a_fire)) 
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator rain.actual is the exogenous rain operator.  Spontaneous rain 
;; has a 33% chance of starting; once it has started, it has a 25% chance 
;; of stopping.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq rain.actual 
	(make-op.actual :name 'rain.actual :pars '()
    :startconds '((= 1 (random 3))) ; 33% chance of rain starting
    :starredStopConds '((= 2 (random 4))) ; 25% chance of stopping after starting
    :starredDeletes '((there_is_rain))
    :adds '((there_is_rain)) 
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_ynq? returns a well-formed formula indicating whether 
;; or not the arg wff is currently in AG's KB, under the closed world 
;; assumption. For example, if AG is currently hungry according to AG's KB,
;; then (is_hungry AG) is returned as the response to 
;; (answer_to_ynq? '(is_hungry AG)); else, (not (is_hungry AG)) is returned.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_ynq? (wff)
	(check-yn-fact-in-kb 'NIL wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_ynq.actual? returns a well-formed formula indicating  
;; whether the arg wff is currently in AG's KB, under the closed world 
;; assumption. In addition, the answer is translated into a proper English 
;; sentence and printed on screen.  For example, if AG is currently hungry 
;; according to AG's KB, then (is_hungry AG) is returned as the response to 
;; (answer_to_ynq.actual? '(is_hungry AG)), and ``AG is hungry'' without the 
;; double quotes is printed.  Otherwise, (not (is_hungry AG)) is 
;; returned and ``it is not the case that AG is hungry'' is printed without 
;; the double quotes.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_ynq.actual? (wff)
	(check-yn-fact-in-kb 'T wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq? returns a collection of well-formed formula(s) 
;; as the answer to the arg wff reflecting what are currently in AG's KB, 
;; under the closed world assumption. Arg wff is a wh-question that has 
;; variables prefixed with ? appearing in slots filled by wh-words.  
;; For example, if AG likes only APPLE1 and BANANA2 according to AG's KB,
;; then ((likes AG APPLE1) (likes AG BANANA2)) is returned as response to 
;; (answer_to_whq? '(likes AG ?wh)). If no answer is found, 
;; then '(not (knows (AG the-answer))) is returned.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq? (wff)
	(check-whq-answer-in-kb 'NIL wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq.actual? returns a collection of well-formed 
;; formula(s) as the answer to the arg wff reflecting what are currently in 
;; AG's KB, under the closed world assumption. Arg wff is a wh-question 
;; with variables prefixed with ? appearing in slots filled by wh-words.  
;; For example, if AG likes only APPLE1 and BANANA2 according to AG's KB,
;; ((likes AG APPLE1) (likes AG BANANA2)) is returned as the response to 
;; (answer_to_whq.actual? '(likes AG ?wh)), and ``AG likes APPLE1'' and ``AG likes 
;; BANANA2'' without double quotes are printed on two lines.  If no answer 
;; is found, '(not (knows (AG the-answer))) is returned and ``it is not the 
;; case that AG knows the answer'' without the double quotes is printed .
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq.actual? (wff)
	(check-whq-answer-in-kb 'T wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_ynq, AG answers the yes-no question ?q asked 
;; by USER.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_ynq 
      (make-op :name 'answer_user_ynq :pars '(?q)
        :preconds '( (wants USER (that (tells AG USER (whether ?q)))) )
        :effects '( (not (wants USER (that (tells AG USER (whether ?q)))))
                    (knows USER (that (answer_to_ynq? ?q)))
			  		)
        :time-required 1
        :value 10
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_ynq.actual, AG answers the yes-no question 
;; ?q asked by USER.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_ynq.actual 
	(make-op.actual :name 'answer_user_ynq.actual :pars '(?q)
	:startconds '( (wants USER (that (tells AG USER (whether ?q)))) )
	:stopconds '( (not (wants USER (that (tells AG USER (whether ?q))))) )
	:deletes '( (wants USER (that (tells AG USER (whether ?q)))) )
	:adds '( ;(knows USER (that (answer_to_ynq?.actual ?q)))				
					 (says+to+at_time AG (that (answer_to_ynq.actual? ?q)) USER (current_time?))
					 (not (wants USER (that (tells AG USER (whether ?q)))))
		   	 )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq, AG answers the wh-question ?q asked by 
;; USER.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq 
	(make-op :name 'answer_user_whq :pars '(?q)
	:preconds '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:effects '( (not (wants USER (that (tells AG USER (answer_to_whq ?q)))))
				(knows USER (that (answer_to_whq? ?q)))
			  )
	:time-required 1
	:value 10
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq.actual, AG answers the wh-question ?q 
;; asked by USER.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq.actual 
	(make-op.actual :name 'answer_user_whq.actual :pars '(?q)
	:startconds '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:stopconds '( (not (wants USER (that (tells AG USER (answer_to_whq ?q))))) )
	:deletes '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:adds	'( ;(knows USER (that (answer_to_whq.actual? ?q)))				
			   (says+to+at_time AG (that (answer_to_whq.actual? ?q)) USER (current_time?))
			   (not (wants USER (that (tells AG USER (answer_to_whq ?q)))))
			 )
	)	
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator walk, AG walks from point ?x to point ?y on road ?z, with 
;; initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq walk 
	(make-op :name 'walk :pars '(?x ?y ?z ?f)
	:preconds '((is_at AG ?x)        
				(is_on ?x ?z)        
				(is_on ?y ?z) (point ?y)
				(navigable ?z)
                (is_tired_to_degree AG ?f) )
    :effects '((is_at AG ?y) 
    		   (not (is_at AG ?x))
               ;(is_tired_to_degree AG (+ ?f 0.5))
               (is_tired_to_degree AG (+ ?f (* 0.5 (distance_from+to+on? ?x ?y ?z))))  
               (not (is_tired_to_degree AG ?f)) )
    :time-required '(distance_from+to+on? ?x ?y ?z)
    :value '(- 3 ?f)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This evaluation function returns the numeric distance from location arg
;; x to location arg y along the path arg z. This function is called by 
;; functions walk.actual and walk.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun distance_from+to+on? (x y z)
	(let	(result pt1 pt2 units index1 index2 str)
			; If both x and y are named road points, simply do a look-up.
		(if (and (evalFunctionPredicate (cons 'point (list x))) (evalFunctionPredicate (cons 'point (list y))))
			(dolist (triple (get x 'next))
				(when (and (eq z (first triple)) (eq y (second triple)))
					(setq result (third triple))
					
					(return-from distance_from+to+on? result)
				)
			)	
			; Otherwise, x is of the form (the_pt+units_from+towards+on_road? ?d ?a ?b ?r), 
			; and parse the result to get the distance.
			(progn
				(if (atom x)
					(setq str (string x))
					(setq str (apply (car x) (cdr x))); (string x))
				)
				(setq index1 (search "PT_" str))
				(setq index2 (search "_UNITS" str))
				(setq units (parse-integer (subseq str (+ index1 3) index2)))
				(setq index1 (search "FROM_" str))
				(setq index2 (search "_TOWARDS" str))
				(setq pt1 (INTERN (string-upcase (subseq str (+ index1 5) index2))))
				(setq index1 (+ index2 9))
				(setq index2 (search "_ON" str))
				(setq pt2 (INTERN (string-upcase (subseq str index1 index2))))
				(if (and (eq pt1 x) (eq pt2 y))
					(return-from distance_from+to+on? (- (distance_from+to+on? pt1 pt2 z) units))
					(return-from distance_from+to+on? units)
				)
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator walk.actual, AG walks from point ?x to point ?y on road ?z,  
;; with initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq walk.actual 
	(make-op.actual :name 'walk.actual :pars '(?x ?y ?z ?f)
	:startconds '((is_at AG ?x)        
				  (is_on ?x ?z)        
				  (is_on ?y ?z) (point y)
				  (navigable ?z)
                  (is_tired_to_degree AG ?f) )
    :stopconds '((not (navigable ?z)) 
    			 (is_at AG ?y) )
    :deletes '((is_at AG ?#1)
    		   (is_tired_to_degree AG ?#2))
    :adds '((is_at AG (the_pt+units_from+towards+on_road? (* 1 (elapsed_time?)) ?x ?y ?z))
    		(is_at AG (the_pt+units_from+towards+on_road? (- (distance_from+to+on? ?x ?y ?z) (* 1 (elapsed_time?))) ?y ?x ?z))
    	    (is_on (the_pt+units_from+towards+on_road? (* 1 (elapsed_time?)) ?x ?y ?z) ?z)
    	    (is_on (the_pt+units_from+towards+on_road? (- (distance_from+to+on? ?x ?y ?z) (* 1 (elapsed_time?))) ?y ?x ?z) ?z)
    		(is_tired_to_degree AG (+ ?f (* 0.5 (elapsed_time?)))) )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator sleep, AG sleeps to relieve his fatigue ?f, but experiences 
;; an increase in his hunger ?h.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sleep 
	(make-op :name 'sleep :pars '(?f ?h) ; level of fatigue ?f 
                                         ; {0, 0.5, 1.0, 1.5, ...}
                                         ; similarly for hunger ?h
    :preconds '((is_at AG home)
                (is_tired_to_degree AG ?f)
                (>= ?f 2.5);(>= ?f 0.5)
                (is_hungry_to_degree AG ?h)
                (> ?f ?h) ; more tired than hungry
                (not (there_is_a_fire))
                (not (there_is_a_flood)) )
    :effects '((is_tired_to_degree AG 0.0)
               (not (is_tired_to_degree AG ?f))
               (is_hungry_to_degree AG (+ ?h (* 0.5 ?f))) )
    :time-required '(* 4 ?f)
    :value '(* 1 ?f)
    )
)
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator sleep.actual, AG sleeps to relieve his fatigue ?f, but 
;; experiences an increase in his hunger ?h.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sleep.actual 
	(make-op.actual :name 'sleep.actual :pars '(?f ?h) ; level of fatigue ?f 
                                                	   ; level of hunger ?h
    :startconds '((is_at AG home)
                  (is_tired_to_degree AG ?f)
                  (>= ?f 2.5)
                  (is_hungry_to_degree AG ?h)
                  (> ?f ?h) ); more tired than hungry
    :stopconds '((there_is_a_fire)
    						 (is_tired_to_degree AG 0.0))
    :deletes '((is_tired_to_degree AG ?#1) 
               (is_hungry_to_degree AG ?#2) )
    :adds '((is_tired_to_degree AG (- ?f (* 0.5 (elapsed_time?))))
            (is_hungry_to_degree AG (+ ?h (* 0.25 (elapsed_time?)))) ) 
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If hungry, at the same location ?y as is an is_edible food item ?x, and 
;; aware of the item being is_edible, then AG can eat the item to assuage his 
;; hunger ?h provided there is no fire or flood. Currently, food items are 
;; inexhaustible.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq eat 
	(make-op :name 'eat :pars '(?h ?x ?y) ; level of hunger ?h
	:preconds '( (is_hungry_to_degree AG ?h) 
				 (>= ?h 2.0)
				 (is_at AG ?y) 
				 (is_at ?x ?y) 
				 (is_edible ?x) 
				 (knows AG (whether (is_edible ?x)))
				 (not (there_is_a_fire))
                 (not (there_is_a_flood)) )
	:effects '( (is_hungry_to_degree AG 0.0) 
				(not (is_hungry_to_degree AG ?h)) )
	:time-required 1
	:value '(* 2 ?h)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?y as is an is_edible food item ?x and aware of 
;; the item being is_edible, and as long as he is hungry, then AG can eat the 
;; item to assuage his hunger ?h provided there is no fire or flood.
;; Currently, food items are inexhaustible.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq eat.actual 
	(make-op.actual :name 'eat.actual :pars '(?h ?x ?y)
	:startconds '( (is_hungry_to_degree AG ?h) 
				   (>= ?h 2.0)
				   (is_at AG ?y) 
				   (is_at ?x ?y) 
				   (is_edible ?x) 
				   (knows AG (whether (is_edible ?x))) )
	:stopconds '( (there_is_a_fire)
				  (there_is_a_flood) 
				  (is_hungry_to_degree AG 0.0) )
	:deletes '( (is_hungry_to_degree AG ?#1) )
	:adds '( (is_hungry_to_degree AG 0.0) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If thirsty, at the same location ?y as is a is_potable drink item ?x, and 
;; aware of it being is_potable, then AG can drink ?x to sate his thirst ?h.
;; Currently, drink items are inexhaustible.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq drink 
	(make-op :name 'drink :pars '(?h ?x ?y) ; level of thirst ?h
	:preconds '( (is_thirsty_to_degree AG ?h) 
				 (> ?h 0.0)
				 (is_at AG ?y) 
				 (is_at ?x ?y) 
				 (is_potable ?x) 
				 (knows AG (whether (is_potable ?x))) 
				 (not (there_is_a_fire))
                 (not (there_is_a_flood)) )
	:effects '( (is_thirsty_to_degree AG 0.0) 
				(not (is_thirsty_to_degree AG ?h)) )
	:time-required 1
	:value '(* 2 ?h)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?y as is a is_potable drink item ?x and aware of 
;; it being is_potable, and as long as he is thirsty, then AG can drink ?x to 
;; sate his thirst ?h. Currently, drink items are inexhaustible.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq drink.actual 
	(make-op.actual :name 'drink.actual :pars '(?h ?x ?y)
	:startconds '( (is_thirsty_to_degree AG ?h) 
				   (> ?h 0.0)
				   (is_at AG ?y) 
				   (is_at ?x ?y) 
				   (is_potable ?x) 
				   (knows AG (whether (is_potable ?x))) )
	:stopconds '( (there_is_a_fire)
				  		(there_is_a_flood) 
				  		(is_thirsty_to_degree AG 0.0) )
	:deletes '( (is_thirsty_to_degree AG ?#1) )
	:adds '( (is_thirsty_to_degree AG 0.0) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?z as is an agent ?x who knows whether ?y holds 
;; which AG does not know, then AG can ask ?x and know whether ?y holds.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ask+whether 
	(make-op :name 'ask+whether :pars '(?x ?y ?z)
	:preconds '( (is_at AG ?z) 
				 (is_at ?x ?z) 
				 (can_talk ?x) 
				 (knows ?x (whether ?y))
				 (not (knows AG (whether ?y))) )
	:effects '( (knows AG (whether ?y)) )
	:time-required 1
	:value 5 
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?z as is an agent ?x who knows whether ?y holds 
;; which AG does not know, then AG can ask ?x and know whether ?y holds.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ask+whether.actual 
	(make-op.actual :name 'ask+whether.actual :pars '(?x ?y ?z)
	:startconds '( (is_at AG ?z) 
				   (is_at ?x ?z) 
				   (can_talk ?x) 
				   (knows ?x (whether ?y))
				   (not (knows AG (whether ?y))) )
	:stopconds '( (knows AG (whether ?y)) )
	:deletes '( )
	:adds '( (knows AG (whether ?y)) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If bored, at the same location ?y as is a is_playable item ?x, and 
;; aware of it being is_playable, then AG can play ?x to relieve his boredom
;; but also experience an increase in both his hunger ?h and fatigue ?f.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq play 
	(make-op :name 'play :pars '(?h ?f ?x ?y) ; level of hunger ?h
	:preconds '( (is_bored AG) 				  ; level of fatigue ?f
				 (is_at AG ?y) 
				 (is_at ?x ?y) 
				 (is_playable ?x) 
				 (is_thirsty_to_degree AG ?h)
                 (is_tired_to_degree AG ?f)
				 (knows AG (whether (is_playable ?x))) )
	:effects '( (not (is_bored AG)) 
				(not (is_thirsty_to_degree AG ?h))
                (not (is_tired_to_degree AG ?f))
				(is_thirsty_to_degree AG (+ ?h 0.5))
                (is_tired_to_degree AG (+ ?f 0.5)) )
	:time-required 1
	:value 3
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?y as is a is_playable item ?x and aware of it 
;; being is_playable, and as long as AG is bored, then AG can play ?x to 
;; relieve his boredom but also experience an increase in both his hunger 
;; ?h and fatigue ?f.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq play.actual 
	(make-op.actual :name 'play.actual :pars '(?h ?f ?x ?y)
	:startconds '( (is_bored AG)
				   (is_at AG ?y) 
				   (is_at ?x ?y) 
				   (is_playable ?x) 
				   (is_thirsty_to_degree AG ?h)
                   (is_tired_to_degree AG ?f)
				   (knows AG (whether (is_playable ?x))) )
	:stopconds '( (not (is_bored AG)) )
	:deletes '( (is_tired_to_degree AG ?#2) 
                (is_thirsty_to_degree AG ?#1) 
                (is_bored AG) )
    :adds '( (is_tired_to_degree AG (+ ?f (* 0.5 (elapsed_time?))))
             (is_thirsty_to_degree AG (+ ?h (* 0.5 (elapsed_time?)))) ) 
	)
)
