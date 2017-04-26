;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Name: gridworld-definitions.lisp
;; LISP ROUTINES FOR CREATING A GRIDWORLD (ROADS AND OBJECTS)
;; 
;; Author: Lenhart Schubert and Daphne Liu
;; Date of Version 1: Jan. 2008 version 1 by Lenhart Schubert
;; Date of Version 2: Apr. 2009 version 2 by Daphne Liu
;; Date of Version 3: Jan. 2010 version 3 by Daphne Liu
;; Date of Version 4: Nov. 2010 version 4 by Daphne Liu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *** SHOULD ADD A FUNCTION FOR ADDING GENERAL KNOWLEDGE INDEPENDENTLY
;; OF def-roadmap, def-object and place-object, SINCE CONDITIONAL
;; KNOWLEDGE OF MORE GENERAL FORMS (E.G., WHERE THE ANTECEDENT IS
;; NOT SIMPLY A TYPE CONSTRAINT) IS PERMITTED IN WORLD KNOWLEDGE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; WE STORE ALL SPECIFIC FACTS IN *WORLD-FACTS*, NOT LOCALLY.
; (General facts are still in *general-knowledge*, though we might
; eventually distinguish general facts from general knowledge)
;
; We have also changed storage of local facts (as 'facts  property
; of locations) to just storing *world-facts*.
; If appropriate, the user needs to supply lists *left-comoving-preds* 
; and *right-comoving-preds*, to allow inference of all local objects.

(defparameter *roadmap-knowledge* (make-hash-table :test #'equal))
; will become a list of map facts when def-roadmap is run           
                                      
(defparameter *general-knowledge* nil)
; will become a list of general facts about object types, and
; possibly other non-unit Horn clauses added by the user                         
                                      
(defparameter *extra-initial-knowledge* (make-hash-table :test #'equal)) 
; Extra initial knowledge that may be supplied to AG when the initial 
; place-obj is done for object AG. (It is "extra" in the sense that AG
; is presumed to know the roadmap knowledge, and also any locally 
; apparent facts at the point where it is placed). The extra initial 
; knowledge should be supplied as curr-facts in the place-obj command,  
; or the user could explicitly add facts to *extra-initial-knowledge*
; before doing 'initialize-state-node'.                                  

(defparameter *world-facts* (make-hash-table :test #'equal))
; Roadmap knowledge plus all other ground facts; these are collected
; together when objects are placed in the gridworld and when the 
; initial state is computed in function 'initialize-state-node' 
; (see "gridworld-planning.lisp")                             
                               
(defparameter *protected-facts* (make-hash-table :test #'equal))
; Initially, before any inferences are done, these are the same 
; as *world-facts*; later, the *protected-facts* are updated with
; the effects of actions actually carried out, but they are not
; augmented with inferences, so that in new states inferences can
; be computed from scratch (avoiding the need to explicitly identify 
; and retract inferences that have become invalid).

(defvar *occluded-preds* '(likes contains is_playable knows is_edible is_potable))
; Predicates for local facts that are not immediately known to AG 
; (unless the first (subject) argument is AG)

(defvar *epistemic-preds* '(knows is_obligated_to believes wants))
; Currently, there's no significance of declaring a predicate as
; being in *epistemic-preds* because the inference routines currently
; handle only the modalities "wants", "knows whether" and "knows that".
(defvar *visited-places* nil)
(defvar *visited-objects* nil)
;(defparameter *roadmap-extrainitial-knowledge-terms* nil)   
                             
;(defparameter *left-comoving-preds* nil)
(defparameter *left-comoving-preds* '(is_in))

;(defparameter *right-comoving-preds* nil)
(defparameter *right-comoving-preds* '(has))

(defun def-roadmap (points roads); Revised Dec. 2009 by Daphne Liu
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Create a graph-like "roadmap", consisting of points (nodes) and
; connections (edges) beween pairs of points. The connections are
; labeled, where we think of these labels as road names; at most
; 2 edges impinging on a node may have any particular name. (Having
; 2 edges with the same name at a node means that we can follow
; a road through an "intersection" -- or, if these are the only
; 2 edges -- past a "point of interest", usually one where some
; object is located.)
;
; points: a list of distinct node names, such as Point1, Point2,
;    Cross-roads1, Dead-end3, etc.;
; roads: a list of lists, each beginning with a road name,
;	 followed by the first point on this road, the distance 
;	 from the first point to the second point on this road, 
;	 the second point on this road, and so on;
;    a road must not reach any point twice, because we want
;    "taking road x from point y to point z" to correspond to
;    a unique path; theoretically, 'points' may include points
;    not reached by any road; a "minimalist" roadmap would be
;    one that has just one point (where we place AG), and no 
;    roads.
;
; METHOD:
; 1. Check to make sure each road segment has exactly two points
;    and a distance), and roads are not self-intersecting; {a road
;    is self-intersecting iff after removal of any duplicate
;    points, the list of points defining the road is shorter};
;    Report any discrepancies between the points given as 
;    first argument and points occurring on roads; (however,
;    such discrepancies are allowed);
;
; 2. Create a graph structure by attaching 'next'
;    properties to points, where such a property consists
;    of a list of (road-name adjacent-point distance) triples; this can
;    be done in an obvious way by sequentially processing
;    the given roads; 
; 	 
; 3. Create the *roadmap-knowledge* list of facts about roads,
;    points, and what points are on what roads.
;
 (let (tuple name pt-dist-list pt-list dist-list curr trimmed-pt-list 
       road-points leng pt1 pt2 currleng isolated-points 
       unlisted-points rr rc)
   (clrhash *extra-initial-knowledge*)
   (clrhash *roadmap-knowledge*)
   (clrhash *world-facts*)
   (clrhash *protected-facts*)
   (setq *general-knowledge* nil) ;(clrhash *general-knowledge*)
   ;(setq *roadmap-extrainitial-knowledge-terms* nil)
   
   (dolist (p points) (remprop p 'next)); cleanup for safety
   (dolist (r roads); error checking
   		(setq name (car r))
   		(setq pt-dist-list (cdr r))
   		(setq pt-list '())
   		(setq dist-list '())
   		(setq leng (length pt-dist-list))
   		(dotimes (i leng t)
			(setq curr (nth i pt-dist-list))
			(if (evenp i)
	   			(if (symbolp curr) 
	   				(push curr pt-list)
	   				(progn 	(format t "~%***Road ~a is missing a valid segment" name)
	            			(return-from def-roadmap 'ERROR-TERMINATION) 
					)
				)
	          	(if (numberp curr)
	            	(push curr dist-list)
	   				(progn 	(format t "~%***Road ~a is missing a valid distance" name)
	            			(return-from def-roadmap 'ERROR-TERMINATION) 
	            	)
	          	) 
			) 
		)
            
   		(setq trimmed-pt-list (remove-duplicates pt-list))
   		(when	(< (length trimmed-pt-list) (length pt-list))
			(format t "~%***Road ~a is self-intersecting" name)
			(return-from def-roadmap 'ERROR-TERMINATION) 
		)
			
		(when	(not (= (- (length pt-list) 1) (length dist-list)))
				(format t "~%***Road ~a is missing a valid segment or distance" name)
				(return-from def-roadmap 'ERROR-TERMINATION) 
		)
		
		(setq road-points (union pt-list road-points))
            	
		; Create a roadmap knowledge list for the Motivated Explorer (AG):
		(add_tuple_to_hashtable(cons 'road (list name)) *roadmap-knowledge* 'NIL)
		(add_tuple_to_hashtable (cons 'navigable (list name)) *roadmap-knowledge* 'NIL)
		
		(setq pt1 (pop pt-list))
		(add_tuple_to_hashtable (list 'is_on pt1 name) *roadmap-knowledge* 'NIL)		
		
		(while (> (length pt-list) 0)
			(setq pt2 (pop pt-list))
			(add_tuple_to_hashtable (list 'is_on pt2 name) *roadmap-knowledge* 'NIL)	
			(setq currleng (pop dist-list))
			(push (list name pt2 currleng) (get pt1 'next))
			(push (list name pt1 currleng) (get pt2 'next)) 
			(setq pt1 pt2)
		)
	)
     
	(setq isolated-points (set-difference points road-points))
	(setq unlisted-points (set-difference road-points points))
	(if isolated-points
		(format t "~%### ISOLATED POINTS: ~a" isolated-points) )
	(if unlisted-points
		(format t "~%### UNLISTED POINTS ON ROADS: ~a" unlisted-points) )
   
	(dolist (p (unionf road-points isolated-points))
		(add_tuple_to_hashtable (list 'point p) *roadmap-knowledge* 'NIL)
	)
	
	(add_htable_to_hashtable *roadmap-knowledge* *world-facts* 'NIL)
	(add_htable_to_hashtable *roadmap-knowledge* *protected-facts* 'NIL)
   
	(push (list (list 'tells '?x (list 'that '?y)) '=> '?y) *general-knowledge*)
	(push (list (list 'knows '?x (list 'that '?y)) '=> '?y) *general-knowledge*)
 )
); end of def-roadmap
                     
;; EXAMPLE OF CREATING ROADMAP: tested summer of 08
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; (def-roadmap '(a b c d e) '((r1 a 1 b 2 c 3 d) (r2 b 4 d)))
;;
;; ### ISOLATED POINTS: (E)
;; NIL
;; [2] CL-USER(29): (format t "~%~a" *roadmap-knowledge*)
;;
;; ((POINT E) (IS_ON D R1) (IS_ON D R2) (POINT D) (IS_ON C R1) 
;;	(POINT C) (IS_ON B R1) (IS_ON B R2) (POINT B) (IS_ON A R1) 
;;	(POINT A) (ROAD R2) (ROAD R1))
;; NIL


(defun def-object (obj-type properties)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; obj-type: an atomic identifier for a type, e.g., 'sasquatch';
; properties: a list of abbreviated general, permanent properties
; such as
;    '(is_animate is_furry (has_IQ 50));
;    This is expanded so as to produce a conditional whose
;    antecedent applies the type predicate to a variable and
;    whose consequent applies the given predicate in each 
;    property first to the variable and then to any additional
;    arguments that are given; thus we will get
;      ((sasquatch ?x) => (is_animate ?x))
;      ((sasquatch ?x) => (is_furry ?x))
;      ((sasquatch ?x) => (has_IQ ?x 50))
; (Note: the originally planned temporal arguments have been
;  omitted. These might still be added, but at this point it
;  seems more convenient to omit them.)
;
 (let ((ante (list obj-type '?x)) conse)
    (dolist (p properties)
       (setq conse 
          (if (atom p)
              (list p '?x)
              (cons (car p) (cons '?x (cdr p))) 
          )
       )
       (push (list ante '=> conse) *general-knowledge*)
    )
 )); end of def-object


;; EXAMPLE OF DEFINING AN OBJECT TYPE:
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [2] CL-USER(32): (def-object 'sasquatch '(is_animate is_furry (has_IQ 50)))
;;
;; [2] CL-USER(33): *general-knowledge*
;;(((SASQUATCH ?X) => (HAS_IQ ?X 50)) ((SASQUATCH ?X) => (IS_FURRY ?X))
;; ((SASQUATCH ?X) => (IS_ANIMATE ?X)))


(defun place-object (name obj-type point time-pt associated-things 
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     curr-facts propos-attitudes); Revised Dec. 2009 by Daphne Liu
;                    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; We name an entity (of a specified type) and place it at
; some point in the Gridworld (and make the fact that it is of
; that type, and `is_at' that point part of the list that is
; the value of the `facts' property of `point'), at a specified
; 'time-pt' and we supply three kinds of additional information 
; for it (where these become available to AG as knowledge "packets"
; if AG is at that point):
; - things that it currently "has" -- a list of typed entities
;   such as ((key key1) (sword sword1) (banana banana3));
;   these may be regarded as *possessions* in the case of animate
;   beings, or as contained or attached objects, in the case of
;   inanimate objects such as trees or boxes; for example,
;   a type predication like (key Key3) supplied under this
;   heading means that the named entity has Key3, and that
;   thing is a key; (see below for the representation of these
;   facts); the type facts, possession facts, and `is_at' facts
;   are all placed on the list comprising the *world-facts*;
; - current-state facts about it; e.g., (hungry Grunt), or
;   (likes Grunt Tweety); in this version we do not add time
;   arguments, but directly place the given facts in *world-facts*. 
;   N.B.: IF THE OBJECT PLACED IS AG, THEN THE CURR-FACTS MAY ALSO
;   INCLUDE FACTS THAT DO *NOT* HAVE AG IN SUBJECT POSITION; THESE
;   ARE TREATED AS "EXTRA" INITIAL KNOWLEDGE OF AG (IN ADDITION TO
;   ROADMAP KNOWLEDGE, SELF-KNOWLEDGE AND ANY ADDITIONAL LOCALLY
;   APPARENT FACTS);
; - propositional attitudes such as (knows Grunt (has Robbie
;   Banana)); another example: (wants Grunt (has Grunt Banana1));
;   it is even possible to have nested knowledge facts or goal
;   facts such as (knows Grunt (knows Robbie (want Grunt (has
;   Grunt Banana1)))), i.e., Grunt knows that Robbie knows that
;   Grunt wants to have the banana; but such complex facts
;   would more likely be produced by inference than by manual
;   input. Again these facts are put in *world-facts*;
;
; Note: `time-pt' is a number such as 0, but this is used in
; the present version of the code just to set *AG-clock*, not in any
; predicates (time remains implicit in predicates),
;
  (let (facts list-of-terms-in-added-tuple)
    ; type predication about 'name':
    (push (list obj-type name) facts) 
    (push (list 'is_at name point) facts)
    ; set global *here* and *AG-clock* parameters if name is 'AG:
    (if (eq name 'AG) (setq *here* point *AG-clock* time-pt))

    (dolist (p associated-things); the things that 'name' "has"
       (push p facts)
       (push (list 'is_at (second p) point) facts)
       (push (list 'has name (second p)) facts) 
    )
    
    (dolist (f (remove-duplicates curr-facts :test #'equal)) 
       (push f facts)
       (when (and (eq name 'AG) (not (eq (second f) 'AG)))
           (add_tuple_to_hashtable f *extra-initial-knowledge* 'NIL)
       )
    )
    
    (dolist (f propos-attitudes) (push f facts))

    (setq facts (remove-duplicates facts :test #'equal) )
    ; In updating *protected-facts*, we could just set them equal
    ; to *world-facts*; but we update them independently just in
    ; case we ever want to place a new object in the gridworld
    ; while already in the middle of a gridworld run.
    (dolist (f facts)
    	(add_tuple_to_hashtable f *world-facts* 'NIL)
    	(add_tuple_to_hashtable f *protected-facts* 'NIL)
    )

    facts ; output the new facts
 )); end of place-object

;; EXAMPLE OF PLACING AN OBJECT:
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [3] CL-USER(52): (place-object 'Grunt 'sasquatch 'b 0 '((banana Banana1)
;;                   (key Key4)) '((hungry Grunt) (likes Grunt Tweety))
;;                  '((knows Grunt (has AG Banana2))))
;;
;; (format t "~%~a" (get 'Grunt 'facts))
;;
;; ((KNOWS GRUNT (HAS AG BANANA2)) (LIKES GRUNT TWEETY)
;;  (HUNGRY GRUNT) (HAS GRUNT KEY4) (HAS GRUNT BANANA1)
;;  (IS_AT GRUNT B) (SASQUATCH GRUNT))
;; NIL
;; [3] CL-USER(54): (format t "~%~a" (get 'Key4 'facts))
;; 
;; ((KEY KEY4))
;; NIL
;; [3] CL-USER(55): (format t "~%~a" (get 'Banana2 'facts))
;; 
;; NIL
;; NIL
;; [3] CL-USER(56): (format t "~%~a" (get 'Banana1 'facts))
;; 
;; ((BANANA BANANA1))
;; NIL

