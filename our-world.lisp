;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Name: our-world.lisp
;; This is the world of TED, an agent created to simulate the effects
;; of inherent personality traits on decisionmaking, planning ability,
;; and emotional modeling. It is built from the original Gridworld, created
;; by Daphne Liu and Lenhart Schubert.
;;
;; Authors: Santiago Loane and Nathaniel Potrepka
;; Original Author: Daphne Liu
;; Date: Apr. 2017 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-roadmap '(home grove plaza cave garden) '((path1 home 3 grove) (path2 home 2 plaza) (path3 grove 2 cave) (path4 home 4 garden)))
(def-object 'robot '(is_animate can_talk))
(def-object 'expert '(is_animate can_talk is_human))
(def-object 'instrument '(is_inanimate is_playable))
(def-object 'juice '(is_inanimate is_potable (has_cost 2.0)))
(def-object 'pizza '(is_inanimate is_edible (has_cost 5.0)))
(def-object 'wolf '(is_animate is_dangerous))
(def-object 'speaker '(is_inanimate is_loud))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Openness: Likes variety & difference. Values different traits from its own
;;           and actions that it has not performed before.
;; Conscientiousness: Plans actions strongly and dislikes spontaneity, or behavior
;;           it deems disruptive. Neat, systematic and wary, strongly dislikes
;;           opposing behavior.
;; Extraversion: Enjoys and seeks out interaction with others. Enjoys any (nonhostile)
;;           form of interaction with other agents, and the more the merrier. Become
;;           upset when without interaction for prolonged periods of time.
;; Agreeableness: Kind, empathetic, wants to help others, tries to cooperate and
;;           enjoys seeing others succeed. Exhibits selfless decisionmaking.
;; Neuroticism: "Moodiness" or anxiety, will respond worse to stress or hostile
;;           behavior, interprets threats with much greater severity.
;;
;; The following values correspond to the above OCEAN traits, ranging across +/- 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setf (gethash 'O *traits*) 0.0)
;(setf (gethash 'C *traits*) 0.0)
;(setf (gethash 'E *traits*) 0.6)
;(setf (gethash 'A *traits*) 1.0)
;(setf (gethash 'N *traits*) 0.0)

(setf (gethash 'hunger *vitals*) 4.0)
(setf (gethash 'thirst *vitals*) 2.0)
(setf (gethash 'fatigue *vitals*) 3.0)

;; Note that we create some "current facts" about
;; TED that are really about the situation at plaza;
;; this is just a way of ensuring that TED knows these
;; facts right at the outset.

(place-object 'TED 'robot 'home 0
  nil ; no associated-things
  ; current facts
  `((is_hungry_to_degree TED ,(gethash 'hunger *vitals*))
    (is_thirsty_to_degree TED ,(gethash 'thirst *vitals*))
    (is_tired_to_degree TED ,(gethash 'fatigue *vitals*))
    (is_alive TED)

    ; OCEAN traits
    (is_open_to_degree TED ,(gethash 'O *traits*))
    (is_conscientious_to_degree TED ,(gethash 'C *traits*))
    (is_extroverted_to_degree TED ,(gethash 'E *traits*))
    (is_agreeable_to_degree TED ,(gethash 'A *traits*))
    (is_neurotic_to_degree TED ,(gethash 'N *traits*))

    ; Emotional state
    (is_happy_to_degree TED 0.0) ;valence
    (is_bored TED)

    (can_talk guru)
    (is_at guru grove)

    (can_talk guru_injured)
    (is_at guru_injured grove)
    (is_injured guru_injured)

    (can_talk guru_grumpy)
    (is_at guru_grumpy plaza)

    (can_talk guru_friendly)
    (is_at guru_friendly plaza)

    (is_at juice3 plaza)
    (is_at pizza3 home)
    (is_at piano2 home)
     ;Note that right after the call to function initialize-state-node,
     ;TED knows (is_edible pizza3) and (is_playable piano2). The reason is
     ;TED knows the types of pizza3 and piano2 colocated with TED at home,
     ;and TED does forward inference using its initial knowledge in
     ;conjunction with *general-knowledge* to derive the knowledge
     ;of the type-specific properties of pizza3 and piano2. All this
     ;occurs despite the general occlusiveness of the predicates
     ;is_edible and is_playable as specified by *occluded-preds*.
     ;This behavior is acceptable. To prevent TED from knowing about
     ;the edibility of pizza3 and playability of piano2 at the outset,
     ;one would need to specify general inference rules from TED to use,
     ;separate from *general-knowledge*. But for simplicity, we specify
     ;only *general-knowledge* as the inference rules both known in the
     ;simulated world and used by TED.
  )
  ; propositional attitudes
  '((knows TED (whether (is_playable piano2)))
    (knows TED (whether (is_edible pizza3)))
    (knows TED (whether (is_injured guru_injured)))
    (knows TED (that (knows guru (whether (is_potable juice3)))))
    ;merely (knows guru (whether (is_potable juice3))) won't work, because (knows guru ...) is first
    ;deposited into *protected-facts* and *world-facts* via place-object, and then later filtered
    ;to see if it should be known (added to local facts) to TED in initialize-state-node. And
    ;guru's knowledge is occluded and so filtered out. So the bug fix for now is that when you want
    ;to initially assign to TED the knowledge of some other agent's knowledge, you should prefix
    ;that with `knows TED that', and hence the form (knows TED (that (knows another_agent ...))).
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
      (is_friendly_to_degree guru 0.0)
     )
)

(place-object 'guru_friendly 'expert 'plaza 0
    nil ; no associated-things
    ; current facts
    '((is_friendly_to_degree guru_friendly 1.0)
     )
    ; propositional attitudes
    nil
)

(place-object 'guru_grumpy 'expert 'plaza 0
    nil ; no associated-things
    ; current facts
    '((is_friendly_to_degree guru_grumpy -1.0)
     )
    ; propositional attitudes
    nil
)

(place-object 'guru_injured 'expert 'grove 0
    nil ; no associated-things
    ; current facts
    '((is_injured guru_injured)
      (is_friendly_to_degree guru_injured 0.5)
     )
    ; propositional attitudes
    nil
)

(place-object 'wolf1 'wolf 'cave 0
    nil ; no associated-things
    ; current facts
    '((is_hungry wolf1)
      (is_dangerous wolf1)
     )
    ; propositional attitudes
    nil
)

(place-object 'speaker2 'speaker 'garden 0
    nil ; no associated-things
    ; current facts
    '((is_loud speaker2)
     )
    ; propositional attitudes
    nil
)

(place-object 'pizza4 'pizza 'cave 0
    nil ; no associated-things
    ; current facts
    '((is_edible pizza4)
     )
    ; propositional attitudes
    nil
)
;(setq *occluded-preds*
;    '(is_playable knows is_edible is_potable)
; We omit this, as *occluded-preds* is currently already set in
; "gridworld-definitions.lisp".

(setq *operators* '(walk eat answer_user_ynq answer_user_whq sleep drink ask+whether play die play+with talk+with heal))

(defun generate-search-beam ()
    (let ((c (gethash 'C *traits*)))
        (cond
            ((> c 0.75 ) (list (cons 6 *operators*) (cons 5 *operators*) (cons 4 *operators*) (cons 3 *operators*)))
            ((> c 0.5  ) (list (cons 5 *operators*) (cons 4 *operators*) (cons 3 *operators*) (cons 2 *operators*)))
            ((> c 0.25 ) (list (cons 5 *operators*) (cons 4 *operators*) (cons 3 *operators*) ))
            ((< c -0.75) (list (cons 2 *operators*) (cons 1 *operators*) ))
            ((< c -0.50) (list (cons 3 *operators*) (cons 2 *operators*) ))
            ((< c -0.25) (list (cons 3 *operators*) (cons 2 *operators*) (cons 1 *operators*) ))
            (t           (list (cons 4 *operators*) (cons 3 *operators*) (cons 2 *operators*) )))))

(setq *search-beam* (generate-search-beam))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator fire.actual is the exogenous fire operator.  As long as there
;; is no rain, a spontaneous fire has a 5% chance of starting; once
;; it has started, it has a 50% chance of stopping, and it also goes out
;; as soon as there is rain.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq fire.actual
    (make-op.actual :name 'fire.actual :pars '()
    :startconds '(('NIL))
                  ;(= 3 (random 20))) ; 5% chance of fire starting
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
    :startconds '(('NIL));(= 1 (random 3))) ; 33% chance of rain starting
    :starredStopConds '((= 2 (random 4))) ; 25% chance of stopping after starting
    :starredDeletes '((there_is_rain))
    :adds '((there_is_rain))
    )
)

(defun is_dead? ()
    (if (>= (+ (gethash 'hunger *vitals*) (gethash 'thirst *vitals*) (gethash 'fatigue *vitals*)) 30.0)
        'T
        'NIL)
    )

(setq die
    (make-op :name 'die :pars '(?u ?h ?f)
        :preconds '((is_alive TED)
                    (is_tired_to_degree TED ?f)
                    (is_thirsty_to_degree TED ?h)
                    (is_hungry_to_degree AH ?u)
                    (>= (+ ?f ?h ?u) 30.0))
        :effects '((is_dead TED)
                    (not (is_alive TED)))
        :time-required 1
        :value -999
        )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exogenous death operator. If hunger, thirst or fatigue exceed 10.0,
;; agent will spontaneously die. Agent is aware of its potential mortality,
;; and will try to avoid death at all costs. An agent with poor planning
;; ability (low C stat) may be careless enough to die.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq die.actual
    (make-op.actual :name 'die.actual :pars '()
    :startconds '((is_alive TED)
                  (is_dead?))
    :starredStopConds '((is_dead TED))
    :stopconds '((is_dead TED))
    :deletes '((is_alive TED))
    :adds '((is_dead TED))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_ynq? returns a well-formed formula indicating whether
;; or not the arg wff is currently in TED's KB, under the closed world
;; assumption. For example, if TED is currently hungry according to TED's KB,
;; then (is_hungry TED) is returned as the response to
;; (answer_to_ynq? '(is_hungry TED)); else, (not (is_hungry TED)) is returned.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_ynq? (wff)
	(check-yn-fact-in-kb 'NIL wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_ynq.actual? returns a well-formed formula indicating
;; whether the arg wff is currently in TED's KB, under the closed world
;; assumption. In addition, the answer is translated into a proper English
;; sentence and printed on screen.  For example, if TED is currently hungry
;; according to TED's KB, then (is_hungry TED) is returned as the response to
;; (answer_to_ynq.actual? '(is_hungry TED)), and ``TED is hungry'' without the
;; double quotes is printed.  Otherwise, (not (is_hungry TED)) is
;; returned and ``it is not the case that TED is hungry'' is printed without
;; the double quotes.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_ynq.actual? (wff)
	(check-yn-fact-in-kb 'T wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq? returns a collection of well-formed formula(s)
;; as the answer to the arg wff reflecting what are currently in TED's KB,
;; under the closed world assumption. Arg wff is a wh-question that has
;; variables prefixed with ? appearing in slots filled by wh-words.
;; For example, if TED likes only APPLE1 and BANANA2 according to TED's KB,
;; then ((likes TED APPLE1) (likes TED BANANA2)) is returned as response to
;; (answer_to_whq? '(likes TED ?wh)). If no answer is found,
;; then '(not (knows (TED the-answer))) is returned.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq? (wff)
	(check-whq-answer-in-kb 'NIL wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq.actual? returns a collection of well-formed
;; formula(s) as the answer to the arg wff reflecting what are currently in
;; TED's KB, under the closed world assumption. Arg wff is a wh-question
;; with variables prefixed with ? appearing in slots filled by wh-words.
;; For example, if TED likes only APPLE1 and BANANA2 according to TED's KB,
;; ((likes TED APPLE1) (likes TED BANANA2)) is returned as the response to
;; (answer_to_whq.actual? '(likes TED ?wh)), and ``TED likes APPLE1'' and ``TED likes
;; BANANA2'' without double quotes are printed on two lines.  If no answer
;; is found, '(not (knows (TED the-answer))) is returned and ``it is not the
;; case that TED knows the answer'' without the double quotes is printed .
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq.actual? (wff)
	(check-whq-answer-in-kb 'T wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_ynq, TED answers the yes-no question ?q asked
;; by USER.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_ynq
      (make-op :name 'answer_user_ynq :pars '(?q)
        :preconds '( (wants USER (that (tells TED USER (whether ?q)))) )
        :effects '( (not (wants USER (that (tells TED USER (whether ?q)))))
                    (knows USER (that (answer_to_ynq? ?q)))
			  		)
        :time-required 1
        :value 100
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_ynq.actual, TED answers the yes-no question
;; ?q asked by USER.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_ynq.actual
	(make-op.actual :name 'answer_user_ynq.actual :pars '(?q)
	:startconds '( (wants USER (that (tells TED USER (whether ?q)))) )
	:stopconds '( (not (wants USER (that (tells TED USER (whether ?q))))) )
	:deletes '( (wants USER (that (tells TED USER (whether ?q)))) )
	:adds '( ;(knows USER (that (answer_to_ynq?.actual ?q)))
					 (says+to+at_time TED (that (answer_to_ynq.actual? ?q)) USER (current_time?))
					 (not (wants USER (that (tells TED USER (whether ?q)))))
		   	 )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq, TED answers the wh-question ?q asked by
;; USER.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq
	(make-op :name 'answer_user_whq :pars '(?q)
	:preconds '( (wants USER (that (tells TED USER (answer_to_whq ?q)))) )
	:effects '( (not (wants USER (that (tells TED USER (answer_to_whq ?q)))))
				(knows USER (that (answer_to_whq? ?q)))
			  )
	:time-required 1
	:value 100
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq.actual, TED answers the wh-question ?q
;; asked by USER.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq.actual
	(make-op.actual :name 'answer_user_whq.actual :pars '(?q)
	:startconds '( (wants USER (that (tells TED USER (answer_to_whq ?q)))) )
	:stopconds '( (not (wants USER (that (tells TED USER (answer_to_whq ?q))))) )
	:deletes '( (wants USER (that (tells TED USER (answer_to_whq ?q)))) )
	:adds	'( ;(knows USER (that (answer_to_whq.actual? ?q)))
			   (says+to+at_time TED (that (answer_to_whq.actual? ?q)) USER (current_time?))
			   (not (wants USER (that (tells TED USER (answer_to_whq ?q)))))
			 )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This evaluation function returns the numeric distance from location arg
;; x to location arg y along the path arg z. This function is called by
;; functions walk.actual and walk.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun distance_from+to+on? (x y z)
    (let    (result pt1 pt2 units index1 index2 str)
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
;; With operator walk, TED walks from point ?x to point ?y on road ?z, with
;; initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq walk
	(make-op :name 'walk :pars '(?x ?y ?z ?f ?o)
	:preconds '((is_alive TED)
        (is_at TED ?x)
				(is_on ?x ?z)
				(is_on ?y ?z) (point ?y)
				(navigable ?z)
        (is_open_to_degree TED ?o)
        (is_tired_to_degree TED ?f) )
    :effects '((is_at TED ?y)
    		   (not (is_at TED ?x))
               ;(is_tired_to_degree TED (+ ?f 0.5))
               (is_tired_to_degree TED (+ ?f (* 0.5 (distance_from+to+on? ?x ?y ?z))))
               (not (is_tired_to_degree TED ?f)) )
    :time-required '(distance_from+to+on? ?x ?y ?z)
    :value '(+ (- 3 ?f) ?o)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator walk.actual, TED walks from point ?x to point ?y on road ?z,
;; with initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq walk.actual
	(make-op.actual :name 'walk.actual :pars '(?x ?y ?z ?f)
	:startconds '((is_alive TED)
          (is_at TED ?x)
				  (is_on ?x ?z)
				  (is_on ?y ?z) (point y)
				  (navigable ?z)
                  (is_tired_to_degree TED ?f) )
    :stopconds '((not (navigable ?z))
    			 (is_at TED ?y) )
    :deletes '((is_at TED ?#1)
    		   (is_tired_to_degree TED ?#2))
    :adds '((is_at TED (the_pt+units_from+towards+on_road? (* 1 (elapsed_time?)) ?x ?y ?z))
    		(is_at TED (the_pt+units_from+towards+on_road? (- (distance_from+to+on? ?x ?y ?z) (* 1 (elapsed_time?))) ?y ?x ?z))
    	    (is_on (the_pt+units_from+towards+on_road? (* 1 (elapsed_time?)) ?x ?y ?z) ?z)
    	    (is_on (the_pt+units_from+towards+on_road? (- (distance_from+to+on? ?x ?y ?z) (* 1 (elapsed_time?))) ?y ?x ?z) ?z)
    		(is_tired_to_degree TED (+ ?f (* 0.5 (elapsed_time?)))) )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator sleep, TED sleeps to relieve his fatigue ?f, but experiences
;; an increase in his hunger ?h.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sleep
	(make-op :name 'sleep :pars '(?f ?h) ; level of fatigue ?f
                                         ; {0, 0.5, 1.0, 1.5, ...}
                                         ; similarly for hunger ?h
    :preconds '((is_alive TED)
                (is_at TED home)
                (is_tired_to_degree TED ?f)
                (>= ?f 2.5);(>= ?f 0.5)
                (is_hungry_to_degree TED ?h)
                (> ?f ?h) ; more tired than hungry
                (not (there_is_a_fire))
                (not (there_is_a_flood)) )
    :effects '((is_tired_to_degree TED 0.0)
               (not (is_tired_to_degree TED ?f))
               (is_hungry_to_degree TED (+ ?h (* 0.5 ?f))) )
    :time-required '(* 4 ?f)
    :value '(* 1 ?f)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator sleep.actual, TED sleeps to relieve his fatigue ?f, but
;; experiences an increase in his hunger ?h.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sleep.actual
	(make-op.actual :name 'sleep.actual :pars '(?f ?h) ; level of fatigue ?f
                                                	   ; level of hunger ?h
    :startconds '((is_alive TED)
                  (is_at TED home)
                  (is_tired_to_degree TED ?f)
                  (>= ?f 2.5)
                  (is_hungry_to_degree TED ?h)
                  (> ?f ?h) ); more tired than hungry
    :stopconds '((there_is_a_fire)
    						 (is_tired_to_degree TED 0.0))
    :deletes '((is_tired_to_degree TED ?#1)
               (is_hungry_to_degree TED ?#2) )
    :adds '((is_tired_to_degree TED (- ?f (* 0.5 (elapsed_time?))))
            (is_hungry_to_degree TED (+ ?h (* 0.25 (elapsed_time?)))) )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If hungry, at the same location ?y as is an is_edible food item ?x, and
;; aware of the item being is_edible, then TED can eat the item to assuage his
;; hunger ?h provided there is no fire or flood. Currently, food items are
;; inexhaustible.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq eat
	(make-op :name 'eat :pars '(?h ?x ?y) ; level of hunger ?h
	:preconds '( (is_alive TED)
         (is_hungry_to_degree TED ?h)
				 (>= ?h 2.0)
				 (is_at TED ?y)
				 (is_at ?x ?y)
				 (is_edible ?x)
				 (knows TED (whether (is_edible ?x)))
				 (not (there_is_a_fire))
                 (not (there_is_a_flood)) )
	:effects '( (is_hungry_to_degree TED 0.0)
				(not (is_hungry_to_degree TED ?h)) )
	:time-required 1
	:value '(* 2 ?h)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?y as is an is_edible food item ?x and aware of
;; the item being is_edible, and as long as he is hungry, then TED can eat the
;; item to assuage his hunger ?h provided there is no fire or flood.
;; Currently, food items are inexhaustible.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq eat.actual
	(make-op.actual :name 'eat.actual :pars '(?h ?x ?y)
	:startconds '( (is_alive TED)
           (is_hungry_to_degree TED ?h)
				   (>= ?h 2.0)
				   (is_at TED ?y)
				   (is_at ?x ?y)
				   (is_edible ?x)
				   (knows TED (whether (is_edible ?x))) )
	:stopconds '( (there_is_a_fire)
				  (there_is_a_flood)
				  (is_hungry_to_degree TED 0.0) )
	:deletes '( (is_hungry_to_degree TED ?#1) )
	:adds '( (is_hungry_to_degree TED 0.0) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If thirsty, at the same location ?y as is a is_potable drink item ?x, and
;; aware of it being is_potable, then TED can drink ?x to sate his thirst ?h.
;; Currently, drink items are inexhaustible.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq drink
	(make-op :name 'drink :pars '(?h ?x ?y) ; level of thirst ?h
	:preconds '( (is_alive TED)
         (is_thirsty_to_degree TED ?h)
				 (> ?h 0.0)
				 (is_at TED ?y)
				 (is_at ?x ?y)
				 (is_potable ?x)
				 (knows TED (whether (is_potable ?x)))
				 (not (there_is_a_fire))
                 (not (there_is_a_flood)) )
	:effects '( (is_thirsty_to_degree TED 0.0)
				(not (is_thirsty_to_degree TED ?h)) )
	:time-required 1
	:value '(* 2 ?h)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?y as is a is_potable drink item ?x and aware of
;; it being is_potable, and as long as he is thirsty, then TED can drink ?x to
;; sate his thirst ?h. Currently, drink items are inexhaustible.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq drink.actual
	(make-op.actual :name 'drink.actual :pars '(?h ?x ?y)
	:startconds '( (is_alive TED)
           (is_thirsty_to_degree TED ?h)
				   (> ?h 0.0)
				   (is_at TED ?y)
				   (is_at ?x ?y)
				   (is_potable ?x)
				   (knows TED (whether (is_potable ?x))) )
	:stopconds '( (there_is_a_fire)
				  		(there_is_a_flood)
				  		(is_thirsty_to_degree TED 0.0) )
	:deletes '( (is_thirsty_to_degree TED ?#1) )
	:adds '( (is_thirsty_to_degree TED 0.0) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?z as is an agent ?x who knows whether ?y holds
;; which TED does not know, then TED can ask ?x and know whether ?y holds.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ask+whether
	(make-op :name 'ask+whether :pars '(?x ?y ?z ?h ?e)
	:preconds '( (is_alive TED)
         (is_extroverted_to_degree TED ?e)
				 (is_at TED ?z)
				 (is_at ?x ?z)
				 (can_talk ?x)
				 (knows ?x (whether ?y))
				 (not (knows TED (whether ?y)))
         (is_happy_to_degree TED ?h) )
	:effects '( (knows TED (whether ?y))
              (is_happy_to_degree TED (min 5.0 (+ ?e ?h)))
              (not (is_happy_to_degree TED ?h)) )
	:time-required 1
	:value '(* 5.0 (+ 1.0 ?e))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?z as is an agent ?x who knows whether ?y holds
;; which TED does not know, then TED can ask ?x and know whether ?y holds.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ask+whether.actual
	(make-op.actual :name 'ask+whether.actual :pars '(?e ?x ?y ?z ?h)
	:startconds '( (is_alive TED)
           (is_extroverted_to_degree TED ?e)
				   (is_at TED ?z)
				   (is_at ?x ?z)
				   (can_talk ?x)
				   (knows ?x (whether ?y))
				   (not (knows TED (whether ?y)))
           (is_happy_to_degree TED ?h) )
	:stopconds '( (knows TED (whether ?y)) )
	:deletes '( (is_happy_to_degree TED ?#1) )
	:adds '( (knows TED (whether ?y))
             (is_happy_to_degree TED (min 5.0 (+ ?e ?h))) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If bored, at the same location ?y as is a is_playable item ?x, and
;; aware of it being is_playable, then TED can play ?x to relieve his boredom
;; but also experience an increase in both his hunger ?h and fatigue ?f.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq play
	(make-op :name 'play :pars '(?h ?f ?x ?y ?o)
	:preconds '( (is_alive TED)
         (is_bored TED)
				 (is_at TED ?y)
				 (is_at ?x ?y)
				 (is_playable ?x)
				 (is_thirsty_to_degree TED ?h)
         (is_tired_to_degree TED ?f)
         (is_open_to_degree TED ?o)
				 (knows TED (whether (is_playable ?x))) )
	:effects '( (not (is_bored TED))
				(not (is_thirsty_to_degree TED ?h))
        (not (is_tired_to_degree TED ?f))
				(is_thirsty_to_degree TED (+ ?h 0.5))
        (is_tired_to_degree TED (+ ?f 0.5)) )
	:time-required 1
	:value '(* 5.0 (+ 1.0 ?o))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?y as is a is_playable item ?x and aware of it
;; being is_playable, and as long as TED is bored, then TED can play ?x to
;; relieve his boredom but also experience an increase in both his hunger
;; ?h and fatigue ?f.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq play.actual
	(make-op.actual :name 'play.actual :pars '(?h ?f ?x ?y ?o)
	:startconds '( (is_alive TED)
           (is_bored TED)
				   (is_at TED ?y)
				   (is_at ?x ?y)
				   (is_playable ?x)
				   (is_thirsty_to_degree TED ?h)
           (is_tired_to_degree TED ?f)
           (is_open_to_degree TED ?o)
				   (knows TED (whether (is_playable ?x))) )
	:stopconds '( (not (is_bored TED)) )
	:deletes '( (is_tired_to_degree TED ?#2)
              (is_thirsty_to_degree TED ?#1)
              (is_bored TED) )
    :adds '( (is_tired_to_degree TED (+ ?f (* 0.5 (elapsed_time?))))
             (is_thirsty_to_degree TED (+ ?h (* 0.5 (elapsed_time?)))) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?z as is a is_human item ?x, and as long as TED is
;; bored, then TED can play with ?x to relieve his boredom but also experience
;; an increase in both his hunger ?h and fatigue ?f. The perceived value is
;; dependant mostly on TED's extroversion ?e and partially on openness ?o
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq play+with
    (make-op :name 'play+with :pars '(?x ?z ?h ?f ?e ?o)
    :preconds '( (is_alive TED)
                 (is_bored TED)
                 (is_at TED ?z)
                 (is_at ?x ?z)
                 (is_human ?x)
                 (is_thirsty_to_degree TED ?h)
                 (is_tired_to_degree TED ?f)
                 (is_extroverted_to_degree TED ?e)
                 (is_open_to_degree TED ?o)
                 (not (is_injured ?x)) )
    :effects '( (not (is_bored TED))
                (not (is_thirsty_to_degree TED ?h))
                (not (is_tired_to_degree TED ?f))
                (is_thirsty_to_degree TED (+ ?h 0.5))
                (is_tired_to_degree TED (+ ?f 0.5)) )
    :time-required 1
    :value '(+ 1.0 (* 5.0 (* 0.75 ?e) (* 0.25 ?o)))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?z as is a is_human item ?x, and as long as TED is
;; bored, then TED can play with ?x to relieve his boredom but also experience
;; an increase in both his hunger ?h and fatigue ?f. ?x must not be injured
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq play+with.actual
    (make-op.actual :name 'play+with.actual :pars '(?x ?z ?h ?f ?e ?o)
    :startconds '( (is_alive TED)
                 (is_bored TED)
                 (is_at TED ?z)
                 (is_at ?x ?z)
                 (is_human ?x)
                 (is_thirsty_to_degree TED ?h)
                 (is_tired_to_degree TED ?f)
                 (is_extroverted_to_degree TED ?e)
                 (is_open_to_degree TED ?o)
                 (not (is_injured ?x)) )
    :stopconds '( (not (is_bored TED)) )
    :deletes '( (is_tired_to_degree TED ?#2)
              (is_thirsty_to_degree TED ?#1)
              (is_bored TED) )
    :adds '( (is_tired_to_degree TED (+ ?f (* 0.5 (elapsed_time?))))
             (is_thirsty_to_degree TED (+ ?h (* 0.5 (elapsed_time?)))) )
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?z as is a is_human item ?x, then TED can talk to
;; ?x to potentially increase its happinness ?h, dependant on agreeableness
;; ?a and/or neuroticism ?n. The perceived value is dependant on TED's
;; extroversion ?e and ?x's friendliness ?f. ?x must not be injured.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq talk+with
    (make-op :name 'talk+with :pars '(?x ?z ?e ?a ?h ?n ?f)
    :preconds '( (is_alive TED)
                 (is_at TED ?z)
                 (is_at ?x ?z)
                 (can_talk ?x)
                 (is_agreeable_to_degree TED ?a)
                 (is_extroverted_to_degree TED ?e)
                 (is_happy_to_degree TED ?h)
                 (is_neurotic_to_degree TED ?n)
                 (not (is_injured ?x))
                 (is_friendly_to_degree ?x ?f))
    :effects '((not (is_bored TED))
               (not (is_happy_to_degree TED ?h))
               (is_happy_to_degree TED (min 5 (+ ?h ?a (min 0 (* ?f ?n))))) )
    :time-required 1
    :value '(+ 1.0 (* 5.0 ?e))

    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?z as is a is_human item ?x, then TED can talk to
;; ?x to potentially increase its happinness ?h, dependant on agreeableness
;; ?a and/or neuroticism ?n.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq talk+with.actual
    (make-op.actual :name 'talk+with.actual :pars '(?x ?z ?e ?a ?h ?n ?f)
    :startconds '( (is_alive TED)
                 (is_at TED ?z)
                 (is_at ?x ?z)
                 (can_talk ?x)
                 (is_agreeable_to_degree TED ?a)
                 (is_extroverted_to_degree TED ?e)
                 (is_happy_to_degree TED ?h)
                 (is_neurotic_to_degree TED ?n)
                 (not (is_injured ?x))
                 (is_friendly_to_degree ?x ?f))
    :stopconds '('T)
    :deletes   '((is_happy_to_degree TED ?#1)
                  (is_bored TED))
    :adds      '((is_happy_to_degree TED (min 5 (+ ?h ?a (min 0 (* ?f ?n))))) )
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?z as is a is_human item ?x, and ?x is_injured,
;; then TED can choose to heal ?x, to later interact with them.
;; dependant on agreeableness ?a slightly on extroversion ?e.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq heal
    (make-op :name 'heal :pars '(?x ?z ?e ?a)
    :preconds '( (is_alive TED)
                 (is_at TED ?z)
                 (is_at ?x ?z)
                 (is_injured ?x)
                 (is_agreeable_to_degree TED ?a)
                 (is_extroverted_to_degree TED ?e))
    :effects '((not (is_injured ?x)))
    :time-required 1
    :value '(+ 1.0 (* 5.0 (* 0.5 ?e) (* 1.5 ?a)))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?z as is a is_human item ?x, and ?x is_injured,
;; then TED can choose to heal ?x, to later interact with them.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq heal.actual
    (make-op.actual :name 'heal.actual :pars '(?x ?z ?e ?a)
    :startconds '( (is_alive TED)
                 (is_at TED ?z)
                 (is_at ?x ?z)
                 (is_injured ?x)
                 (is_agreeable_to_degree TED ?a)
                 (is_extroverted_to_degree TED ?e))
    :stopconds '((not (is_injured ?x)))
    :deletes '((is_injured ?x))
    :adds '()
    )
)
