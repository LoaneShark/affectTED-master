;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; HASHTABLE CODE, SIMULATOR CODE, FACT-CHECKING CODE, and 
;; QUESTION-ANSWERING CODE
;; 
;; Author: Daphne Liu
;; Date of Version 1: Feb. 2009 version 1 by Daphne Liu
;; Date of Version 2: Aug. 2009 version 2 by Daphne Liu
;; Date of Version 3: Jan. 2010 version 3 by Daphne Liu
;; Date of Version 4: Nov. 2010 version 4 by Daphne Liu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HASHTABLE CODE 
;; Author: Daphne Liu
;; Date: Jan. 2010
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN MEMB (A LAT)
	(COND ((NULL LAT)   NIL)
		  ((EQ A (CAR LAT))   T)
		  (T   (MEMB A (CDR LAT))) 
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function converts the wff arg pred and returns the corresponding 
;; hashkey.
;; For example, the hashkey for '(not (p a ?x c)) is '(not p a NIL c).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun convert_pred_to_hashkey (pred)
	(if (atom pred) 
		pred
		(mapcar #'gen_hashkey_symbol (flatten pred))
	)	
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function converts the atomic arg symb, returning symb if it is not 
;; a variable, but returning 'NIL otherwise.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gen_hashkey_symbol (symb)
	(if (var symb)
		'NIL
		symb
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function generates all corresponding hashkeys for the given hashkey 
;; arg thekey, and returns a list of thekey plus all the generated hashkeys.
;; Example 1: 
;; CL-USER(8): (generate_allkeys_from_hashkey '(NOT P A B C))
;;  ((NOT P) (NOT P A NIL NIL) (NOT P NIL B NIL) (NOT P NIL NIL C)
;;  (NOT P NIL NIL NIL) (NOT P A B C))
;; Example 2: 
;; CL-USER(9): (generate_allkeys_from_hashkey '(NOT P NIL B C))
;;  ((NOT P) (NOT P NIL B NIL) (NOT P NIL NIL C) (NOT P NIL NIL NIL) 
;;  (NOT P NIL B C))
;; Example 3: 
;; CL-USER(10): (generate_allkeys_from_hashkey '(NOT P NIL))
;;  ((NOT P) (NOT P NIL))
;; Example 4: 
;; CL-USER(11): (generate_allkeys_from_hashkey '(NOT p a))
;;  ((NOT P) (NOT P NIL) (NOT P A))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun generate_allkeys_from_hashkey (thekey)
	(let  ((result (list thekey))
		   (num-elements 0)
		   (startposn 1)
		   (counter 0)
		   (first-arg-posn 1)
		   (last-arg-posn 1)
		   (init-key-seq 'NIL)
		   (new-key 'NIL)
		   (accum 'NIL)
		   (curr-arg 'NIL)
		   posn-reified
		   reified-keys
		  )
		  (when (atom thekey) (return-from generate_allkeys_from_hashkey result))
		  (setq num-elements (length thekey))
		  (when (or (<= num-elements 1) (and (= num-elements 2) (eq (car thekey) 'not)))	
		  		(return-from generate_allkeys_from_hashkey result)
		  )
		  
		  (setq posn-reified (position 'that thekey))
		  (when (numberp posn-reified)
		  	(setq new-key (first-n thekey posn-reified))
		  	(push (append new-key '(nil)) result)
		  	(push (append new-key '(that nil)) reified-keys)
		  )
		  (setq posn-reified (position 'whether thekey))
		  (when (numberp posn-reified)
		  	(setq new-key (first-n thekey posn-reified))
		  	(push (append new-key '(nil)) result)
		  	(push (append new-key '(whether nil)) reified-keys)
		  )
		  
		  (when (null reified-keys)
		  	(push thekey reified-keys)
		  )
		  
		  (dolist (key reified-keys)  		
		  	  (setq num-elements (length key) 
		  	  		that-posns 'NIL
		  	  		whether-posns 'NIL)
		  
			  (if (eq (car key) 'not) 
			  		(progn
			  			(setq startposn 2)
			  			(decf num-elements 2)
			  			(setq init-key-seq (list (first key) (second key)))	
			  			  			
			  		)
			  		(progn
			  			(setq startposn 1)
			  			(decf num-elements 1)
			  			(setq init-key-seq (list (first key)))
			  		)
			  )		  
		  
			  (setq first-arg-posn startposn)
			  (setq last-arg-posn (- (+ first-arg-posn num-elements) 1))
			  (setq new-key init-key-seq)
			  (push new-key result)
			  
			  (setq counter first-arg-posn)
			  (while (<= counter last-arg-posn)
			  	(cond ((eq (nth counter key) 'that)
			  				(push counter that-posns)
			  		  )
			  		  ((eq (nth counter key) 'whether)
			  				(push counter whether-posns)
			  		  )
			  	)
			  	(incf counter 1)
			  )
			  			  	  
			  (dotimes (i num-elements)   ;;;
			  	(setq curr-arg (nth startposn key))
			  		(setq counter first-arg-posn)
			  		(setq accum 'NIL)
			  		(while (< counter startposn)		
			  			(case (nth counter key)
			  				('that (setq accum (append accum '(that))))
			  				('whether (setq accum (append accum '(whether))))
			  				(t (setq accum  (append accum '(nil)))) 
			  			)
			  			(incf counter 1)
			  		)
			  		
			  		(setq new-key (append init-key-seq (append accum (list curr-arg))))
			  		(setq accum 'NIL)
			  		(setq counter (+ startposn 1))
			  		
			  		(while (<= counter last-arg-posn)
			  			(case (nth counter key)
			  				('that (setq accum (append accum '(that))))
			  				('whether (setq accum (append accum '(whether))))
			  				(t (setq accum  (append accum '(nil))))
			  			)
			  			(incf counter 1)
			  		)
			  		
			  		(setq new-key (append new-key accum))	  				  				
			  		(push new-key result)
			  	(incf startposn 1)
			  )
			  (when (> num-elements 0)
					(setq counter first-arg-posn)
		  			(setq accum 'NIL)
		  			(while (<= counter last-arg-posn)			  			
			  			(case (nth counter key)
			  				('that (setq accum (append accum '(that))))
			  				('whether (setq accum (append accum '(whether))))
			  				(t (setq accum  (append accum '(nil))))
			  			)
			  			
				  		(incf counter 1)
		  			)
		  			(setq new-key (append init-key-seq accum))
			  		(push new-key result)
			  )
		 )
			  	
		 (remove-duplicates result :test #'equal)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function returns true if arg key contains the reification symbol 
;; rei-op, and return 'NIL otherwise.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun contains_reified (rei-op key)
	(cond ((atom key) 'NIL)
		  ((numberp (position rei-op key)) 'T)
		  (t 'NIL)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function flattens the list arg lst, returning a single list of 
;; atoms with no nested lists (i.e., with all nesting parentheses removed).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten (lst)
   (cond ((null lst) lst)
         ((listp (car lst))
          (append (flatten (car lst)) (flatten (cdr lst))))
         (t (cons (car lst) (flatten (cdr lst))))
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function adds the wff arg tuple to the hashtable arg table, also 
;; collecting and returning a list of all terms appearing in tuple if
;; both input arg to-collect-terms is 'T and tuple is a *new* addition to 
;; table (not already in table). Otherwise, no terms are collected and an 
;; empty list is returned. Note that if this function adds tuple to table,
;; then table remains changed upon the return from this function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Daphne: Revised 10/11/2012 and fixed a previous error where only one copy of
;         all terms in tuple was being tallied up, regardless of how many times
;         the tuple was really being added to table.
; Daphne: Revised 10/10/2012 to handle new representation of terms
(defun add_tuple_to_hashtable (tuple table to-collect-terms)
	(let ((keys (generate_allkeys_from_hashkey (convert_pred_to_hashkey tuple)))
	      new-hash-value old-hash-value  all-terms (ct 0))
		 
             (dolist (key keys)
                (setq old-hash-value (gethash key table))
                (if (not (null old-hash-value))
                    (prog2 (setq new-hash-value 
                              (unionf (list tuple) (cdr old-hash-value)))
                           (when (and (equal (car new-hash-value) tuple) 
                                      (not (equal tuple (second old-hash-value))))
                                 (setf (gethash key table) 
                                       (cons (+ 1 (car old-hash-value)) 
                                             new-hash-value))
                                 (when (eq to-collect-terms 'T)
                                       (incf ct 1) )))
                    (prog2 (setf (gethash key table) (list 1 tuple))
                           (when (eq to-collect-terms 'T) (incf ct 1)))))
             (if (> ct 0)
                 (make-into-list-of-counts-unique-terms (args tuple) ct) 'NIL)
 )); end of add_tuple_to_hashtable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function adds the list of wffs, 'list-of-tuples', to the hashtable,
;; table, also collecting and returning a list of all terms appearing in
;; all those tuples in list-of-tuples newly added to table (not already in
;; table) if the argument 'to-collect-terms' is T. Otherwise, no terms are 
;; collected and an empty list is returned. Note that if this function 
;; adds any tuples in list-of-tuples to table, then table remains changed
;; upon the return from this function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun add_list_of_tuples_to_hashtable (list-of-tuples table to-collect-terms)
  (let (list-of-terms-in-added-tuples)
       (dolist (tuple list-of-tuples)
          (setq list-of-terms-in-added-tuples 
                (merge_term_list_with_term_list 
                   (add_tuple_to_hashtable tuple table to-collect-terms)
                   list-of-terms-in-added-tuples)))
       list-of-terms-in-added-tuples
 )); end of add_list_of_tuples_to_hashtable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function creates and returns a new hashtable whose contents are 
;; the same as those of the hashtable arg old-htable. Note that old-htable
;; remains unchanged upon the return from this function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy_construct_hashtable (old-htable)
	(let ((new-htable (make-hash-table :test #'equal)))
		(add_htable_to_hashtable old-htable new-htable 'NIL)
		new-htable
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function creates and returns a new hashtable whose contents are 
;; the list-of-wffs arg facts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-htable (facts)
  (let ((result (make-hash-table :test #'equal)))    
	(dolist (tuple facts)
  	 	(add_tuple_to_hashtable tuple result 'nil)
  	)
	result
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function takes a list of possibly duplicate terms given by arg
;; lst-of-terms, and returns a new list containing (count unique_term) pairs
;; of each unique term with its respective count. The count is by default 1,
;; as given by arg ct.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Daphne: Revised 10/11/2012 to add the default parameter.
; Daphne: Created 10/10/2012 to handle new representation of terms
;         as (occurrence-count term) pairs
(defun make-into-list-of-counts-unique-terms (lst-of-terms  &OPTIONAL (ct 1))
	(let ((result-term-lst 'NIL))
		(dolist (arg lst-of-terms)
			(setq result-term-lst (merge_term_list_with_term_list (list (list ct arg)) result-term-lst))
		)
		result-term-lst		
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function adds the contents of the hashtable arg to-be-added-htable 
;; to the hashtable arg table, also collecting and returning a list of all
;; terms appearing in all those tuples in to-be-added-htable newly added to
;; table (not already in table) if input arg to-collect-terms is 'T. 
;; Otherwise, no terms are collected and an empty list is returned. Note 
;; that if this function adds any tuples in to-be-added-htable to table, 
;; then table remains changed upon the return from this function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Daphne: Revised 10/11/2012 and fixed a previous error where cdr-to-be-added-value
;         wasn't properly initialized.
; Daphne: Revised 10/10/2012 to handle new representation of terms
(defun add_htable_to_hashtable (to-be-added-htable table to-collect-terms)
	(let (old-hash-value new-hash-value new-hash-length old-hash-length
		  (list-of-terms-in-added-tuples 'NIL)
		  cdr-old-hash-value cdr-to-be-added-value
		 )
		(loop for to-be-added-key being the hash-keys of to-be-added-htable using (hash-value to-be-added-value)
	     	do 
	     	(setq old-hash-value (gethash to-be-added-key table))
(setq cdr-to-be-added-value (cdr to-be-added-value))
	     	(if (not (null old-hash-value))
	     		(progn
	     			(setq cdr-old-hash-value (cdr old-hash-value))
	     			(setq new-hash-value (unionf cdr-to-be-added-value cdr-old-hash-value))
	     			(setq old-hash-length (car old-hash-value))
	     			(setq new-hash-length (length new-hash-value))
	     			(setf (gethash to-be-added-key table) (cons new-hash-length new-hash-value))
	     			(when (and (eq to-collect-terms 'T) (> new-hash-length old-hash-length))			
	     				(setq list-of-terms-in-added-tuples 
	     					(merge_term_list_with_term_list
	     						(make-into-list-of-counts-unique-terms
		     						(collect-terms-duplicate 
		     							(set-differencef cdr-to-be-added-value cdr-old-hash-value)
		     						)
		     					)
	     						list-of-terms-in-added-tuples 
	     					)
	     				)
	     			)
	     		)
	     		(prog2
	     			(setf (gethash to-be-added-key table) to-be-added-value)
	     			(when (eq to-collect-terms 'T)
	     				(setq list-of-terms-in-added-tuples 
	     					(merge_term_list_with_term_list
	     						(make-into-list-of-counts-unique-terms
	     							(collect-terms-duplicate cdr-to-be-added-value)
	     						)
	     						list-of-terms-in-added-tuples
	     					)
	     				)
	     			)
	     		)
	     	)
	    )
	    list-of-terms-in-added-tuples
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function removes the wff arg, tuple, from the hashtable arg, table, 
;; also collecting and returning a list of all terms appearing in tuple if
;; both input arg to-collect-terms is 'T and tuple is indeed already in 
;; table. Otherwise, no terms are collected and an empty list is returned. 
;; Note that if this function removes tuple from table, then table remains
;; changed upon the return from this function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Daphne: Revised 10/11/2012 and fixed a previous error where only one copy of
;         all terms in tuple was being tallyed up, regardless of how many times
;         the tuple was really being removed from table.
; Daphne: Revised 10/10/2012 to handle new representation of terms
(defun remove_tuple_from_hashtable (tuple table to-collect-terms)
	(let ( (keys (generate_allkeys_from_hashkey (convert_pred_to_hashkey tuple)))
	        new-hash-value old-hash-value (ct 0)
	     )
	      
		(dolist (key keys)
			(setq old-hash-value (gethash key table))
			(when (not (null old-hash-value))
				(setq new-hash-value (remove tuple (cdr old-hash-value) :test #'equal))
				(when (< (length new-hash-value) (car old-hash-value))
					(when (eq to-collect-terms 'T)
						(incf ct 1)
					)
					(if (null new-hash-value)
						(remhash key table)
						(setf (gethash key table) (cons (- (car old-hash-value) 1) new-hash-value))
					)
				)
			)
		)
		(if (> ct 0)
			(make-into-list-of-counts-unique-terms (args tuple) ct)
			'NIL
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function removes the list-of-wffs arg list-of-tuples from the 
;; hashtable arg table, also collecting and returning a list of all terms 
;; appearing all those tuples in list-of-tuples removed from table (indeed 
;; already in table) if input arg to-collect-terms is 'T. Otherwise, no 
;; terms are collected and an empty list is returned. Note that if this 
;; function removes any tuples in list-of-tuples from table, then table 
;; remains changed upon the return from this function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Daphne: Revised 10/10/2012 to handle new representation of terms
(defun remove_list_of_tuples_from_hashtable (list-of-tuples table to-collect-terms)
	(let (list-of-terms-in-removed-tuples)
		(dolist (tuple list-of-tuples)
			(setq list-of-terms-in-removed-tuples 
				(merge_term_list_with_term_list (remove_tuple_from_hashtable tuple table to-collect-terms)
						list-of-terms-in-removed-tuples 
				)
			)
		)
		list-of-terms-in-removed-tuples		
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function merges two lists of terms given by args lst1 and lst2,
;; returning the resulting term list with counts updated.
;; Each list of terms is a list of (count unique_term) pairs, and it is
;; assumed that each list of terms arg already has no duplicate terms
;; appearing in it and that each count is a positive integer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun merge_term_list_with_term_list (lst1 lst2)
  (if (null lst1) (return-from merge_term_list_with_term_list lst2))
  (if (null lst2) (return-from merge_term_list_with_term_list lst1))
  (let* (count_of_term the_term index new_count 
         found_pair (resulting_lst lst2)
         (lst_of_terms (mapcar #'second resulting_lst)))

        (dolist (pair lst1)
           (setq count_of_term (first pair))
           (setq the_term (second pair))
           (setq index (position the_term lst_of_terms))
           (if (numberp index)
               (prog2 (setq found_pair (nth index resulting_lst))
                      (setq resulting_lst 
                         (substitute 
                            (list (+ (first found_pair) count_of_term) the_term) 
                            found_pair resulting_lst)))
               (prog2 (setq resulting_lst (cons pair resulting_lst))
                      (push the_term lst_of_terms))))
        resulting_lst
 )); end of merge_term_list_with_term_list

;Nondestructive    Destructive
;--------------    -----------
;REMOVE            DELETE
;REMOVE-IF         DELETE-IF
;REMOVE-IF-NOT     DELETE-IF-NOT
;SUBSTITUTE        NSUBSTITUTE
;SUBSTITUTE-IF     NSUBSTITUTE-IF
;SUBSTITUTE-IF-NOT NSUBSTITUTE-IF-NOT
;REMOVE-DUPLICATES DELETE-DUPLICATES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function removes the first list of terms given by arg lst1 from
;; the second list of terms given by arg lst2, returning the resulting
;; list of terms with counts updated and 0-count terms removed.
;; Each list of terms is a list of (count unique_term) pairs, and it is
;; assumed that each list of terms arg already has no duplicate terms
;; appearing in it, and that each count is a positive integer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Daphne: Created 10/10/2012 to handle new representation of terms
(defun remove_term_list_from_term_list (lst1 lst2)
	(when (OR (null lst1) (null lst2)) (return-from remove_term_list_from_term_list lst2))
	(let* (count_of_term   the_term   index    new_count 
		   found_pair new_count
		   (resulting_lst lst2)
		   (lst_of_terms (mapcar #'second resulting_lst)))
		(dolist (pair lst1)
			(setq count_of_term (first pair))
			(setq the_term (second pair))
			(setq index (position the_term lst_of_terms))
			(when (numberp index)
				(setq found_pair (nth index resulting_lst))

;(format t "pair ~A  found pair ~A ~%" pair found_pair)				
				(setq new_count (- (first found_pair) count_of_term))
				(if (> new_count 0)
					(setq resulting_lst (substitute (list new_count the_term) found_pair resulting_lst))
					(prog2 (setq resulting_lst (remove found_pair resulting_lst :test #'equal))
						   (setq lst_of_terms (delete the_term lst_of_terms)); delete is not always guaranteed to be destructive
					)
				)
			)
;(format t "resulting_lst ~A  lst_of_terms ~A ~%" resulting_lst lst_of_terms)
		)
		resulting_lst
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIMULATOR CODE 
;; Author: Daphne Liu
;; Date: Jan. 2010
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The measure of total net utility that AG has gained thus far.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *total-value* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The queue of currently active events for the simulator execution engine 
;; to process, initialized to the empty list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *event-queue* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The real clock in the simulated world, initialized to 0 and incremented 
;; by one per step.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *real-clock* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The timer keeping track of the currently active physical action 
;; being simulated, initialized to 0 and incremented by one per step.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *event-timer* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The structure template for the actual version of an operator, where most 
;; fields are self-explanatory, name specifies the particular action, 
;; instance specifies the particular action instance, pars specifies 
;; the parameters, startconds specifies the formulas that must be true 
;; of the world (according to the agent's KB) for the operator to be 
;; applicable, stopconds specifies the formulas that will cause the 
;; action instance to terminate when any of them becomes true of the world 
;; (according to the agent's KB), deletes specifies formulas removed 
;; from the world (and the agent's KB), adds specifies formulas added 
;; to the world (and the agent's KB) after application of deletes, 
;; starredStopConds specifies the "abnormal" termination conditions that 
;; will cause the action instance to terminate when any of them becomes 
;; true of the world (according to the agent's KB), starredDeletes 
;; specifies the formulas removed from the world (and the agent's KB) when 
;; any of the termination conditions becomes true of the world 
;; (according to the agent's KB), and starredAdds specifies the formulas 
;; added to the world (and the agent's KB) after application of 
;; starredDeletes when any of the termination conditions becomes true 
;; of the world (according to the agent's KB).  
;; When the startconds are true of the world (according to the agent's KB), 
;; an action can be instantiated and the action instance becomes active.  
;; While the action instance is active, at each time step, both the 
;; stopconds and starredStopConds are checked to see if any of their 
;; formulas becomes true of the world (according to the agent's KB).  
;; If any of the starredStopConds formulas is true, then starredDeletes 
;; and then starredAdds are applied and the action instance is terminated 
;; and no longer active.  If any of the stopconds formulas is true and 
;; none of the starredStopConds formulas are, then no changes are applied 
;; and the action instance is terminated and no longer active.  In all 
;; other cases, deletes and then adds will be applied to the world 
;; (and the agent's KB) and the action instance remains active.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct op.actual
	name
	instance
	pars
	startconds
	stopconds
	deletes
	adds
	starredStopConds
	starredDeletes
	starredAdds
);

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This evaluable function returns the current real clock in the simulated 
;; world.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun current_time? ( )
	*real-clock*
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This evaluable function returns the elapsed time of currently active 
;; physical action being simulated.  This is only used in the actual 
;; version of operators.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elapsed_time? ( )
	*event-timer*
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This evaluable function returns the price of input arg x according to 
;; the world knowledge, or 0 if such information is not found in the 
;; world knowledge.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun price_of? (x)
	(let	((result 0))
		(dolist (wf *world-facts*)
			(when (= 3 (length wf))
					(when (and (eq 'has_cost (first wf)) (eq x (second wf)))
						(setq result (third wf))
						(return-from price_of? result)
					)
			)
		)
		result
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function generates a named state-node as a child of the given 
;; state-node specified by arg state-node-name by applying to the given 
;; state the effects of the given external action specified by arg 
;; action-name. A (generated) name of the new state node is returned.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Daphne: Revised 10/18/2012 to handle new representation of terms
(defun generate-ext-op-state-node (action-name state-node-name)
  (let* ((action (eval action-name))
         (additions (op.actual-adds action))
         (deletions (op.actual-deletes action))
         (state-node (eval state-node-name))
         (old-wff-htable (state-node-wff-htable state-node))
         (local-value (state-node-local-value state-node))
         (new-state-node-name (gensym "STATE-NODE"))
         new-local-value
         new-forward-value
         (new-wff-htable (copy_construct_hashtable old-wff-htable))
         (new-terms (state-node-terms state-node))
        )
    ; Remove deletions that are the same as additions.
    (setq deletions (set-differencef deletions additions))
    ; Remove additions that are the same as existing wffs.
    ;(setq additions (set-differencef additions wffs))
    ; Only real changes are counted in 'new-local-value,'
    ; the inherent value of the new state.
    (setq new-local-value
          (state-value additions deletions local-value))
          
    ; The expected future rewards/costs are calculated for action 
    ; sequences (and their effects), starting at the new state.
    ; This seems like a candidate for learning, based on experience in 
    ; starting from similar states  But we'll have to settle for some 
    ; sort of heuristic guess for now -- maybe the same value for all 
    ; states (with an "optimistic" bias); later this value gets 
    ; replaced by back-propagated values, based on action sequences 
    ; generated from the new state.
    (setq new-forward-value (expected-rewards old-wff-htable))
    
    ; Update the set of wffs to reflect the new state.
	(setq new-terms (remove_term_list_from_term_list  
		(remove_list_of_tuples_from_hashtable deletions new-wff-htable 'T) 
		new-terms)
	)
	(setq new-terms (merge_term_list_with_term_list
		(add_list_of_tuples_to_hashtable additions new-wff-htable 'T)
		new-terms)
	)
    
    ; ** At this point we *should* insert forward inferencing (in case
    ; ** there are inference rules associated with dynamic properties)
    ; ** This would use `all-inferences', as in `initialize-state-node'.
    ; ** We would have to distinguish (actual) *protected-facts* from
    ; ** presumed ones in a new 'protected-facts' field in state-nodes.
    ; ** However, because inferencing is rather inefficient at present
    ; ** we omit it for now, doing it only in "actual" new states.
    (set new-state-node-name
         (make-state-node
            :name new-state-node-name
            :terms new-terms
            :wff-htable new-wff-htable
            :children nil
            :operators nil
            :parent (cons action-name state-node-name)
            :local-value new-local-value
            :forward-value new-forward-value ))

    (state-node-wff-htable (eval new-state-node-name)); debugging
    new-state-node-name
	)
); end of generate-ext-op-state-node

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function forms an instance of given action type op.actual by 
;; applying unification bindings given by arg uni, returning a (generated) 
;; name of the instance, as a variant of the name.  The function is 
;; formulated in such a way tha it could be used equally well for partial 
;; instantiation as for full instantiation of an operator.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun instantiate-op.actual (op.actual uni)
	(if (null uni) (return-from instantiate-op.actual nil))
	(let*	((name (op.actual-name op.actual))
			 	 (instance (gensym (string name)))
				 (pars (op.actual-pars op.actual))
			 	 (startconds (op.actual-startconds op.actual))
			 	 (stopconds (op.actual-stopconds op.actual))
			 	 (deletes (op.actual-deletes op.actual))
			 	 (adds (op.actual-adds op.actual))
			 	 (stdeletes (op.actual-starredDeletes op.actual))
			 	 (stadds (op.actual-starredAdds op.actual))
			 	 (ststopconds (op.actual-starredStopConds op.actual))
				)
		  
		(when (not (eq uni t)); not the trivial unifier
			(dolist (u uni)
				(setq pars (subst (cdr u) (car u) pars)))
			(dolist (u uni)
				(setq startconds (subst (cdr u) (car u) startconds)))
			(dolist (u uni)
				(setq stopconds (subst (cdr u) (car u) stopconds)))
			(dolist (u uni)
				(setq ststopconds (subst (cdr u) (car u) ststopconds)))
			(dolist (u uni)
				(setq adds (subst (cdr u) (car u) adds)))
			(dolist (u uni)
				(setq deletes (subst (cdr u) (car u) deletes)))
			(dolist (u uni)
				(setq stadds (subst (cdr u) (car u) stadds)))
			(dolist (u uni)
				(setq stdeletes (subst (cdr u) (car u) stdeletes)))
		)
			
		(set instance
			(make-op.actual :name name
					:instance instance
					:pars pars
					:startconds startconds
					:stopconds stopconds
					:deletes deletes
					:adds adds
					:starredStopConds ststopconds
					:starredDeletes stdeletes
					:starredAdds stadds
			)
		)
		instance
	)
); end of instantiate-op.actual

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This procedure handles the external spontaneous (rain or fire) operator 
;; named by the given op-name.  If the given arg conds-checked is true, 
;; then the startconds of the given operator already are true of the world 
;; and need not be checked again.  Otherwise, the startconds need be 
;; checked.  The startconds must be true in order to execute the operator.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handleFireAndRain (op-name conds-checked)
	(let*	((op (eval op-name))
			 (startconds (op.actual-startconds op))
			 (adds (op.actual-adds op))
			 (deletes (op.actual-deletes op))
			 (stopconds (op.actual-stopconds op))
			 (bindings 'NIL) instances state-nodes implied-facts record
			 children ; as a list of (action . state) pairs 
			 event
			 evaledStartConds
			)
				
		; Check whether startconds are true of the world and 
		; set bindings accordingly.

(format t "~% handleFireAndRain ~a ~a" (op.actual-name (eval op-name)) conds-checked)		
				
		(if	(eq 'T conds-checked)
			(setq bindings '(T))
			(progn ;prog2
				(setq evaledStartConds (mapcar #'(lambda (x) (evalFunctionPredicateExt x)) startconds))
;(format t "~%evaledStartConds = ~a~%" evaledStartConds)
				(when (eq 'NIL
				 		(or 
				 			(eq 'T (eval (cons 'memb (list (quote 'UNKNOWN) (list 'quote evaledStartConds)))))
							(eq 'T (eval (cons 'memb (list (quote 'NIL) (list 'quote evaledStartConds)))))
						)
					  )
					(setq bindings '(T))
				)
			)
		)

		; Execute the operator only if its startconds are true of the world.
		(when (equal bindings '(T))
(format t " on~%")
			(setq instances 
					(mapcar #'(lambda (u) (instantiate-op.actual op u)) bindings))
			(setq state-nodes (mapcar #'(lambda (i) 
					(generate-ext-op-state-node i *curr-state-node*)) instances))
			(setq children (mapcar #'cons instances state-nodes))
			(setf (state-node-children *curr-state-node*)  children)
			(setf (state-node-operators *curr-state-node*) (list op-name))
			(setf (state-node-forward-value *curr-state-node*) 
					(inclusive-value (car children)))
			
			(setq *curr-state-node* (eval (cdar children)))
			(setq adds (mapcar #'simplify-value adds))
			(setq deletes (mapcar #'simplify-value deletes))
			(setq deletes (set-differencef deletes adds))
			
			(remove_list_of_tuples_from_hashtable deletes *protected-facts* 'NIL)
			(add_list_of_tuples_to_hashtable adds *protected-facts* 'NIL)
			
			(setq *world-facts* (all-inferences *protected-facts* 
					*general-knowledge* *inference-limit*))
			(add_htable_to_hashtable *protected-facts* *world-facts* 'NIL)
			
			; Modify *curr-state-node* to update the agent's beliefs in light of the
			; locally evident facts (and beliefs that have evidently become false).
			(setq new-state (copy_construct_hashtable (notice-new-local-facts *curr-state-node*)))
			(setq event (cons (caar children) new-state))
			
			; Place this external event logged with the current real time of the 
			; simualted world in the queue of active events in the simulated world.
			(setq *event-queue* (append *event-queue* 
						(list (cons event *real-clock*))))
			
			; Log this external event with the current real time of the simulated 
			; world in *real-history* and *AG-history*.
			(push (list event *real-clock*) *real-history*)
			(push (list event *AG-clock*) *AG-history*)
			(setq *states* (cons new-state (cdr *states*)))
		)
	)
); end of handleFireAndRain

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This procedure handles the external spontaneous (rain and fire) 
;; operators.  If an active external event is already in *event-queue*, 
;; its termination conditions (both normal and abnormal) are checked to 
;; see if it can be extended for another time step, and it is extended for 
;; another time step only if its none of its termination conditions hold 
;; true in the world KB.  On the other hand, if an external operator is 
;; not currently an active event in *event-queue*, its startconds are 
;; checked to see if it can be instantiated as an active event, and it is  
;; instantiated as an active event in *event-queue* only if its startconds 
;; hold true in the world KB.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Daphne: Revised 10/18/2012 to handle new representation of terms
; Daphne: Revised Dec. 2009 to handle new representation of wff-htable
(defun handleExtOps ()
	(let	((is-fire 'NIL) 
			 (is-rain 'NIL) 
			 (is-fire-handled 'NIL)
			 (is-rain-handled 'NIL)
			 (is-abn 'NIL) 
			 (is-terminated 'NIL) op name new-terms 
			 (new-wff-htable (state-node-wff-htable *curr-state-node*))
			 ststopconds stopconds adds deletes stadds stdeletes queue-length
			)

		(setq queue-length (length  *event-queue*))

		; Process the currently active external events in *event-queue* one at 
		; at time, extending an event for another time step only if none of 
		; its termination conditions are true in the world KB.
		(dotimes (i queue-length)
			(setq op (caar (pop *event-queue*)))
			(setq name (op.actual-name (eval op)))
(format t "~%HEOEventName ~a ~a" name (length *event-queue*))
			(setq adds (op.actual-adds (eval op)))
			(setq deletes (op.actual-deletes (eval op)))
			(setq stadds (op.actual-starredAdds (eval op)))
			(setq stdeletes (op.actual-starredDeletes (eval op)))
			(setq ststopconds (op.actual-starredStopConds (eval op)))
			(setq stopconds (op.actual-stopconds (eval op)))
			(setq is-abn 'NIL)
			(setq is-terminated 'NIL)
			
			(if (equal name 'fire.actual)
				(progn 
					(setq is-fire 'T)
					(setq is-fire-handled 'T)
					(setq is-rain 'NIL)
				)
				(when (equal name 'rain.actual)
					(setq is-rain 'T)
					(setq is-rain-handled 'T)
					(setq is-fire 'NIL)
				)
			)
			
			; Evaluate whether the termination conditions of the current 
			; active event are true in the world KB.
			(if (eq 'T 
							(eval (cons 'memb (list (quote 'T) 
							(list 'quote (mapcar #'(lambda (x) (evalFunctionPredicateExt x)) ststopconds))
							)))
					)
					(setq is-abn 'T)
					(when (eq 'T 
								(eval (cons 'memb (list (quote 'T) 
								(list 'quote (mapcar #'(lambda (x) (evalFunctionPredicateExt x)) stopconds))
								)))
								)
								(setq is-terminated 'T)
					)
			)
			
			; Handle starred adds and starred deletes for the case where 
			; some of the termination conditions are true in the world KB.
			(if (or (eq 'T is-terminated) (eq 'T is-abn))
				(progn; start of then clause of applying starredAdds and starredDeletes
					; Insert into history as completed. (maybe to-do?)
					; Update the world KB and the agent's KB with starred 
					; adds and starred deletes.
(format t "~% HEOExt ~a Is Terminated." name)			
					(setq stadds (mapcar #'simplify-value stadds))
					(setq stdeletes (mapcar #'simplify-value stdeletes))
					(setq stdeletes (set-differencef stdeletes stadds))
					
					(remove_list_of_tuples_from_hashtable stdeletes *protected-facts* 'NIL)
					(add_list_of_tuples_to_hashtable stadds *protected-facts* 'NIL)
					(setq *world-facts* (all-inferences *protected-facts* 
								*general-knowledge* *inference-limit*))
					(add_htable_to_hashtable *protected-facts* *world-facts* 'NIL)
					
					(setq new-terms (state-node-terms *curr-state-node*))
					(setq new-wff-htable (state-node-wff-htable *curr-state-node*))
					(setq new-terms	(remove_term_list_from_term_list
						(remove_list_of_tuples_from_hashtable stdeletes new-wff-htable 'T)
						new-terms)
					)
					(setq new-terms (merge_term_list_with_term_list
						(add_list_of_tuples_to_hashtable stadds new-wff-htable 'T)
						new-terms)
					)
					
					(setf (state-node-terms *curr-state-node*) new-terms)	
					; Modify *curr-state-node* to update the agent's beliefs 
					; in light of the locally evident facts (and beliefs that 
					; have evidently become false).
					(setq new-state (copy_construct_hashtable (notice-new-local-facts *curr-state-node*)))
					(setq *states* (cons new-state (cdr *states*)))		
				); end of then clause of applying starredAdds and starredDeletes
				
				; Extend the current active event for another time step since 
				; none of its termination conditions are true in the world KB.
				(if	(eq 'T is-fire)
					(handleFireAndRain fire.actual 'T)
					(when (eq 'T is-rain)
						(handleFireAndRain rain.actual 'T)
					)
				)
				
			); end of if-then-else clause for 
			
		); end of processing active external events in *event-queue*

		; Handle the external fire operator only if it has not been handled.
		(when (eq 'NIL is-fire-handled)
			(handleFireAndRain fire.actual 'NIL)
		)
		
		; Handle the external rain operator only if it has not been handled.
		(when (eq 'NIL is-rain-handled)
			(handleFireAndRain rain.actual 'NIL)
		)
	)
); end of handleExtOps

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACT-CHECKING CODE 
;; Author: Daphne Liu
;; Date: Jan. 2010
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function evaluates the function or predicate given by arg wff 
;; using the agent's KB and returns the result of the evaluation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun evalFunctionPredicate (wff)
	(let ((wf wff))
		(when (listp wff)
			(setq wf (mapcar #'simplify-value wff))
		)
	
		
		(cond 	
			((and (listp wf) (evaluable-func (car wf)))
				(apply (car wf) (cdr wf))
			)
			(T (check-fact-in-kb wf (state-node-wff-htable *curr-state-node*))
			)
		)
	)
); end of evalFunctionPredicate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function evaluates the function or predicate given by arg wff 
;; using the world KB and returns the result of the evaluation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun evalFunctionPredicateExt (wff)
	(let ((wf wff))
		(when (listp wff)
			(setq wf (mapcar #'simplify-value wff))
		)
		(cond 	
			((and (listp wf) (evaluable-func (car wf)))
				(apply (car wf) (cdr wf))
			)
			(T (check-fact-in-kb wf *world-facts* 'T)
			)
		)
	)
); end of evalFunctionPredicateExt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function epistemic-fact returns a truth value indicating whether arg wff
;; is an epistemic predication.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun epistemic-fact (wff)
  (cond ((atom wff) (member wff *epistemic-preds*)); unexpected
        (t (and (member (car wff) *epistemic-preds*)
                (not (null (cdr wff)))
           )
        )
  )
); end of epistemic-fact

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function check-fact-in-kb returns a truth value indicating whether arg 
;; wff is true, false, or unknown according to arg kb. This function is 
;; called by functions evalFunctionPredicateExt, evalFunctionPredicate, 
;; and 4.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-fact-in-kb (wff kb &OPTIONAL (is-world-kb 'NIL)); Revised Dec. 2009 by Daphne Liu
	(let	((answer 'NIL)
			)
			
			; If wff is an atom or an empty list, return truth value immediately.
			(when (eq 'T (or (eq wff 'T) (eq wff 'NIL)))
				(return-from check-fact-in-kb wff)
			)
				
			; If wff is a non-T and non-NIL atom, return NIL assuming wff is ill-formed.
			(when (atom wff)
				(return-from check-fact-in-kb answer)
			)
			
			; Assume at this point wff is guaranteed to be a well-formed list wff.
			(setq first-in-wff (car wff))
				
			(cond	((eq first-in-wff 'not)
							(if (gethash (convert_pred_to_hashkey wff) kb)
								(setq answer 'T)	; Check if this negated wff is in kb already.
								(prog2 	
									(setq answer (check-fact-in-kb (second wff) kb is-world-kb))
									(cond ((eq answer 'T)
												(setq answer 'NIL)
										  ) 
										  ((eq answer 'NIL)
												(setq answer 'T)
										  )
										  (t (setq answer 'UNKNOWN))															
									)
								)
							)
					)
					; Handle the case of this +ve wff being in kb already.
					((gethash (convert_pred_to_hashkey wff) kb)
							(setq answer 'T)
					)
					; Handle the case of this +ve, not in kb, wff being epistemic.
					((eq first-in-wff 'knows)
							(setq answer (check-epistemic-fact-in-kb wff kb is-world-kb))
					)
					; Handle the case of this nonepistemic, +ve wff not being in kb.
					(t
						(if is-world-kb
							(setq answer 'NIL)
							(prog2
								(setq subj (second wff))
								(if (eq subj 'AG)
									(setq answer 'NIL)
									(if (member subj *visited-objects*)
										(if (member first-in-wff *occluded-preds*)
											(setq answer 'UNKNOWN)
											(setq answer 'NIL)
										)
										(if (eq 'NIL subj) ;for (not (there_is_rain)) to be true
											(setq answer 'NIL)
											(setq answer 'UNKNOWN)
										)								
									)
								)											
							)
						)
					)
			)
				
			answer
	)
); end of check-fact-in-kb


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function check-epistemic-fact-in-kb returns a truth value indicating 
;; whether arg wff is true, false, or unknown in arg kb. This function is
;; called by function check-fact-in-kb. Arg wff is assumed to be a non-atom,
;; a non-empty list, and an epistemic prediation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-epistemic-fact-in-kb (wff kb &OPTIONAL (is-world-kb 'NIL))
	(let 	((answer 'NIL)
			 (wff-key (convert_pred_to_hashkey wff))
			 ;(is_agent_kb (equal kb (state-node-wff-htable *curr-state-node*)))
			 ;(is_agent_contemplated_kb (equal kb (state-node-wff-htable *node-now*)))
			 (first-in-wff (car wff))
			 (subj (second wff))
			 (third-in-wff (third wff))
			)
				
			(when (eq first-in-wff 'knows)
				(cond	((not (null (gethash wff-key kb)))
							(setq answer 'T)
						)
						((atom third-in-wff)
							(if (eq subj 'AG)
								(setq answer (member third-in-wff *visited-objects*))
								(if is-world-kb
									(setq answer 'NIL)
									(setq answer 'UNKNOWN) ;is_agent_kb is_agent_contemplated_kb
								)
							)
						)
						((eq 'whether (car third-in-wff))
							(if (eq subj (second (second third-in-wff)))
								(setq answer 'T)
								(if is-world-kb
									(setq answer 
										(and (eq subj 'AG) 
											 (not (eq 'UNKNOWN (check-fact-in-kb (second third-in-wff) (state-node-wff-htable *curr-state-node*))))))
									(if (eq subj 'AG)
										(setq answer (not (eq 'UNKNOWN 
															  (check-fact-in-kb (second third-in-wff) kb))))
										(setq answer 'UNKNOWN)
									)
								)
							)
						)
						((eq 'that (car third-in-wff))
							(if (eq subj (second (second third-in-wff)))
								(setq answer (eq 'T (check-fact-in-kb (second third-in-wff) kb is-world-kb)))
								
								(if (eq subj 'AG)
									(prog2
										(setq answer (eq 'T (check-fact-in-kb (second third-in-wff) kb is-world-kb)))
										(when is-world-kb
											(setq answer (and answer
															  (eq 'T (check-fact-in-kb (second third-in-wff) (state-node-wff-htable *curr-state-node*)))))
											
										)
									)
									(if is-world-kb
										(setq answer 'NIL) 
										(if (eq 'T (check-fact-in-kb (second third-in-wff) kb))
											(setq answer 'UNKNOWN)
											(setq answer 'NIL)
										)
									)
								)
							)
						)
						(t (if is-world-kb (setq answer 'NIL) (setq answer 'UNKNOWN))
						)
				)				
			)
				
			answer
	)
); end of check-epistemic-fact-in-kb

(defvar *is-actual* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function check-yn-fact-in-kb returns a well-formed formula indicating  
;; whether arg wf is currently in arg kb, under the closed world 
;; assumption. In addition, the answer is translated into a proper English 
;; sentence and printed on screen iff arg is_actual is true. This function 
;; is called by functions answer_to_ynq?, answer_to_ynq.actual?, and 
;; peer-into-state.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-yn-fact-in-kb (is_actual wf kb &OPTIONAL (is-world-kb 'NIL))
	(let* (;(is-world-kb (equal kb *world-facts*))
		  (*is-actual* is_actual)
		  (ans (check-fact-in-kb wf kb is-world-kb))
		 )

;(format t "wff is ~a ~%" wf)
(setq *is-actual* 'NIL)


		(cond	((eq ans 'T)
					(setq ans wf)
				)
				((eq ans 'NIL)
					(setq ans (list 'not wf))
				)
				(t	(setq ans 'UNKNOWN))
		)
		
;(format t "ans is ~a ~%" ans)		
		(when (eq is_actual 't)
			(if (eq ans 'UNKNOWN)
				(prog2	(verbalize ans wf)
						(format t "~%~%For the question ~a, AG does not currently know the answer as it may be about an occluded property of a local object or about a non-local object.~%" wf)
					
				)
				(prog2	(verbalize ans)
						(when (not is-world-kb)
							(format t "~%For the question ~a, according to AG's current knowledge base, AG offers the answer above.~%" wf)
						)
				)
			)
		)


		ans
	)	   
); end of check-yn-fact-in-kb

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function check-whq-answer-in-kb returns a collection of well-formed 
;; formula(s) answering the wh-question given as arg wf by checking against 
;; arg kb, under the closed world assumption. In addition, the answer is 
;; translated into a proper English sentence and printed on screen iff arg 
;; is_actual is true.  This function is called by functions answer_to_whq?, 
;; answer_to_whq.actual?, and peer-into-state.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-whq-answer-in-kb (is_actual wf kb)
	(let 	((unifiers '()) (result-wffs '()) (is_negated 'NIL) (wff wf) 
			 (is_agent_kb (equal kb (state-node-wff-htable *curr-state-node*)))
			)
;(format t "wff = ~%~A~%" wff)				
		
			(when (eq (car wf) 'not)
				(setq wff (second wf))
				(setq is_negated 'T)
			)
			; wff at this point must be a positive literal.
				
			;(setq unifiers	(remove-if #'degenerate-binding 
			;	(all-bindings-of-goals-to-fact-htable (list wff) 
			;					(state-node-wff-htable *curr-state-node*)
			;					(state-node-terms *curr-state-node*)
			;	)
			;				)
			;)	
			(setq unifiers (all-bindings-of-posgoal-to-fact-htable wff kb))
				
			(setq unifiers (remove-duplicates unifiers :test #'equal))
;(format t "unifiers = ~%~A~%" unifiers)				
			(if (null unifiers)
				(when (eq is_actual 'T)
					(if is_agent_kb
						(prog2
							(verbalize 'UNKNOWN wff 'T)
							(if (eq is_negated 'NIL)
								(format t "~%~%For the question ~a, there are no positive instances that AG knows of, so AG assumes nothing as the answer.~%" wf);;(setq result-wffs '(not (knows (AG the-answer))))
								(format t "~%~%For the question ~a, there are no positive instances that AG knows of, so AG assumes everything as the answer.~%" wf)
							)
						)
						(if (eq is_negated 'NIL)
							(format t "~%~%For the question ~a, there are no positive instances, so nothing is the answer.~%" wf)
							(format t "~%~%For the question ~a, there are no positive instances, so everything is the answer.~%" wf)
						)
					)
				)
				(progn
					(setq result-wffs (MAPCAR #'(LAMBDA (ELEMENT) (SUBST-UNIFIER ELEMENT wff)) unifiers))
					(when (eq is_actual 'T)
						(MAPCAR #'(LAMBDA (ELEMENT) (VERBALIZE ELEMENT)) result-wffs)
						(if is_agent_kb
							(if (eq is_negated 'NIL)
								(format t "~%For the question ~a, other than the above positive instance(s) that AG knows of, AG assumes nothing else as the answer.~%" wf) ;(setq result-wffs '(not (knows (AG the-answer))))
								(format t "~%For the question ~a, other than the above positive instance(s) that AG knows of, AG assumes everything else as the answer.~%" wf)
							)
							(if (eq is_negated 'NIL)
								(format t "~%For the question ~a, other than the above positive instance(s), nothing else is the answer.~%" wf)
								(format t "~%For the question ~a, other than the above positive instance(s), everything else is the answer.~%" wf)
							)
						)
					)
;(setq result-wffs (nth (random (length result-wffs)) result-wffs))
				)
			)
			result-wffs
	)
); end of check-whq-answer-in-kb


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUESTION-ANSWERING CODE 
;; Author: Daphne Liu
;; Date: Jan. 2010
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parseIntoPred (x)
	(cond ((or (not (listp x)) (null x)) x)
		  ((eq 'not (first x)) (list 'not (parseIntoPred (second x))))
		  (t (cons (second x) (cons (first x) (nthcdr 2 x))))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function listen! is invoked before function go! to elicit input by 
;; the user, using functions like `tell', `ask-yn', and `ask-wh'.  An 
;; acceptable input is a list of zero or more sublists, with each sublist 
;; being one of function calls `tell', `ask-yn', and `ask-wh'. If the user 
;; has no input to impart to AG, then the user should simply enter () 
;; without quotes at the prompt and then hit the enter key.  Otherwise, 
;; an example user input would be ((ask-yn (AG is_tired )) 
;; (ask-wh (AG likes ?wh)) (tell (USER is_thankful_to AG))) with 
;; no quotes. USER and AG are the default subjects or objects in the input.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Daphne: Revised 10/9/2012 to handle new representation of terms
(defun listen! ()
	(let ((user-input 'NIL) (lst 'NIL) (implied-facts 'NIL) 
		  (tell-lst 'NIL) (neglst 'NIL) curr-tell curr-ans
		  (new-terms (state-node-terms *curr-state-node*))
		  user-input-intention
		  (new-wff-htable (state-node-wff-htable *curr-state-node*))
		 )
		(format t "You're welcome to ask AG a question or tell it a fact.~%")
		(setq user-input (read))
		(setq user-input (mapcar #'(lambda(y) (list (first y) (parseIntoPred (second y)))) user-input))
		(when (and (listp user-input) (not (null user-input)))
			(dolist (i user-input)
				(cond 
					((eq 'ask-yn (car i))
						(setq user-input-intention (list 'wants 'USER (list 'that (list 'tells 'AG 'USER (list 'whether (second i))))))
						(push user-input-intention lst)
						(push (list 'not user-input-intention) neglst)
					)
					((eq 'ask-wh (car i))   
						(setq user-input-intention (list 'wants 'USER (list 'that (list 'tells 'AG 'USER (list 'answer_to_whq (second i))))))
						(push user-input-intention lst)
						(push (list 'not user-input-intention) neglst)
					)
					((eq 'tell (car i))
						(push (second i) tell-lst)
					)
				)
			)
			
			(setq tell-lst (nreverse tell-lst))
			
			(while tell-lst
				(setq curr-tell (pop tell-lst))
				(setq curr-ans (check-yn-fact-in-kb 'NIL curr-tell *world-facts* 'T))
				(if (equal curr-ans curr-tell)
					(prog2
						(push (list 'tells 'USER (list 'that curr-tell)) lst)
						(if (eq (first curr-tell) 'not)
							(setq neglst (unionf neglst (second curr-tell)))
							(setq neglst (unionf neglst (list (list 'not curr-tell))))
						)
					)
					(cond ((eq curr-ans 'UNKNOWN)
							(format t "~%Your assertion ~a has an unknown truth value in the simulated world so it is not added under the closed world assumption.~%" curr-tell)
						  )
						  (t ;(eq curr-ans 'NIL)
							(format t "~%The negation of your assertion ~a is true in the simulated world so your assertion is not added to avoid introducing inconsistency.~%" curr-tell)
						  )
					)
						;(format t "Are you sure you want to add it (enter y or n)?~%" curr-tell)
						;(setq user-input (read))
						;(when (eq user-input 'y)
						;))
				)		
			)
			
			(setq lst (nreverse lst))
			;(setq *protected-facts* (set-differencef *protected-facts* neglst))
			(remove_list_of_tuples_from_hashtable neglst *protected-facts* 'NIL)
			
			(add_list_of_tuples_to_hashtable lst *protected-facts* 'NIL)
			(setq *world-facts*  (all-inferences *protected-facts* *general-knowledge* *inference-limit*))
			(add_htable_to_hashtable *protected-facts* *world-facts* 'NIL)
			
			(setq new-terms (remove_term_list_from_term_list
				(remove_list_of_tuples_from_hashtable neglst new-wff-htable 'T)
				new-terms)
			)
			(setq new-terms (merge_term_list_with_term_list
				(add_list_of_tuples_to_hashtable lst new-wff-htable 'T)
				new-terms)
			)
			
			(setf (state-node-terms *curr-state-node*) new-terms)
			(setq new-state (copy_construct_hashtable (notice-new-local-facts *curr-state-node*)))
			(setq *states* (cons new-state (cdr *states*)))
		)
		T
	)
); end of listen!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function peer-into-state takes input arg ques of the form '(YNQ wff) or 
;; '(WHQ wff) where wff is a well-formed formula.  For ques of the first 
;; form, the function returns a well-formed formula indicating whether wff
;; is currently in AG's mental state, under the closed world assumption.  
;; For ques of the second form, the function returns a collection of 
;; well-formed formula(s) answering the wh-question wff by checking against
;; AG's mental state, under the closed world assumption. Also, the response 
;; is translated into an English sentence(s) and printed. This top-level 
;; function can be invoked directly at the prompt by the user.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun peer-into-state (ques)
	(cond 	((eq (car ques) 'YNQ)
				(check-yn-fact-in-kb 'T (second ques) (state-node-wff-htable *curr-state-node*))
			)
			((eq (car ques) 'WHQ)
				(check-whq-answer-in-kb 'T (second ques) (state-node-wff-htable *curr-state-node*))
			)
	)
); end of peer-into-state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This evaluable function returns an atomic symbol coerced from a 
;; concatenated string indicating the arg distance between the start 
;; arg and the destination arg along the path arg.  It assumes that 
;; the latter three arguments have already been `simplified.'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun the_pt+units_from+towards+on_road? (distance start destination path)
	(let*	((simplified-dist (simplify-value distance))
				 (dist-str (write-to-string simplified-dist))
			 	 (start-str (string start))
			 	 (dest-str (string destination))
			 	 (path-str (string path))
			 	 (result 'NIL))
		(cond	((= 0 simplified-dist)
							(setq result start))
					((= simplified-dist (distance_from+to+on? start destination path))
							(setq result destination))
					(t 
							(setq result (INTERN (string-upcase 
									(concatenate 'string "the_pt_" dist-str "_units_from_" 
										start-str "_towards_" dest-str "_on_road_" path-str)))))
		)
		result
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function verbalize takes a well-formed formula as input arg wff,  
;; translates it to an English sentence and prints it on the screen.
;; This function for verbalizing a well-formed formula should be invoked 
;; in lieu of the function verbalize-wff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun verbalize (wff &OPTIONAL (input-wff 'NIL) &OPTIONAL (print-var 'NIL))
;(format t "input-wff verbalize is ~a ~%" input-wff)
	(if (and (eq wff 'UNKNOWN) (not (null input-wff)))
		(format t "~%~%~A~%" (concatenate 'string "AG DOES NOT KNOW WHETHER " (simplifyString input-wff print-var)))
		(format t "~%~%~A~%" (simplifyString wff))
	)
); end of verbalize

(defun replaceVarWithAnything (wff any-symb)
	(cond ((atom wff) (if (var wff) any-symb wff))
		  (t (cons (replaceVarWithAnything (first wff) any-symb) (replaceVarWithAnything (cdr wff) any-symb)))
	)
)

(defun simplifyString (wff &OPTIONAL (print-var 'NIL))
	(let* (answer result-lst-strs
		   (result-str "") posn)
		
;(format t "wff simplify-string is ~a ~a ~%" wff print-var)		
		(if (eq 'T print-var)
			(setq answer (verbalize-wff 'NIL (replaceVarWithAnything wff 'ANYTHING-non-AG) 'T)) 
			(setq answer (verbalize-wff 'NIL wff 'T)) 
		)

;(format t "answer simplify-string is ~a~%" answer)
		
		(setq result-lst-strs (mapcar #'(lambda(x) (write-to-string x)) answer))
		   
		(dolist (i result-lst-strs)
			(setq result-str (concatenate 'string (concatenate 'string result-str i) " "))
		)
		(setq result-str (concatenate 'string (string-trim " " result-str) "."))
		(setq posn (search "IT IS NOT THE CASE THAT IT IS NOT THE CASE THAT " result-str))
		(while (numberp posn)
			(setq result-str (subseq result-str (+ posn 48)))
			(setq posn (search "IT IS NOT THE CASE THAT IT IS NOT THE CASE THAT " result-str))
		)
		
;(format t "~%~%~A~%" result-str)
		result-str
	)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function verbalize-wff takes a well-formed formula as input arg wff, 
;; translates it to an English sentence while accumulating the sentence in 
;; the list arg result, and prints it on the screen. This function is 
;; called only by function verbalize.
; Example:
; > (verbalize-wff '() '(is_at AG (the_pt+units_from+towards+on_road? ?d ?x ?y ?z)))
; > (AG IS AT THE PT D UNITS FROM X TOWARDS Y ON ROAD Z)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun verbalize-wff (result wff right-after-that-whether)	
	(cond	((null wff)
					result
			)
			((not (listp wff))
				(append result  (SPLIT-STRING-TO-SYMBOLS (string wff)))
			)
			((eq (car wff) 'not) 
				(verbalize-wff (append result (list 'it 'is 'not 'the 'case 'that)) (cadr wff) (listp (cadr wff)))
			)
			((OR (eq (car wff) 'that) (eq (car wff) 'whether))
				(verbalize-wff (append result (list (car wff))) (cadr wff) (listp (cadr wff)))
			)
			((listp (car wff))
				(verbalize-wff (verbalize-wff result (car wff) right-after-that-whether) (cdr wff) (listp (cadr wff)))
			)
			((numberp (car wff))
				(append result (list (car wff)))
			)
			((not (search "+" (string (car wff))))
				(if (>= (length wff) 2)
					(if right-after-that-whether
						(verbalize-wff 
							(verbalize-wff (verbalize-wff result (second wff) 'NIL) (first wff) 'NIL)
							(nthcdr 2 wff)
							'NIL
						);It's better to test whether (second wff) is a pred; and only if so do we reverse second and first.
						(verbalize-wff 
							(verbalize-wff (verbalize-wff result (first wff) 'NIL) (second wff) 'NIL)
							(nthcdr 2 wff)
							'NIL
						)
					)
					(append result (SPLIT-STRING-TO-SYMBOLS (string (first wff)))
					)
				)
			)
			( t ;(> (search "+" (string (car wff))) 0)
				(if (null result) ; topmost parse
					(append result 
						(append (SPLIT-STRING-TO-SYMBOLS (string (second wff)))
								(SPLIT-STRING-TO-SYMBOLS-INSERT-ARGS-AT-PLUS (STRING (car wff)) (nthcdr 2 wff))
						)
					)
					(append result 
						(SPLIT-STRING-TO-SYMBOLS-INSERT-ARGS-AT-PLUS (STRING (car wff)) (rest wff))
					)
				)
				
			)
	)
); end of verbalize-wff

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function SPLIT-STRING-TO-SYMBOLS-INSERT-ARGS-AT-PLUS takes a well-formed
;; formula string as input arg STRING, a list of arguments as arg ARG-LIST, 
;; and an optional arg of tokenizer symbols in double quotes. The function 
;; translates STRING into an English sentence by removing spaces, by 
;; replacing underscores and dashes and question marks with spaces, and 
;; by replacing plus signs with arguments in ARG-LIST.
;; translates it to an English sentence while accumulating the sentence in 
;; the list arg result, and prints it on the screen. This function is 
;; called by function verbalize-wff and was modified from function 
;; SPLIT-STRING of the following source:
;; <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;; http://darcs.informatimago.com/darcs/public/lisp/common-lisp/string.lisp
; Example:
; > (SPLIT-STRING-TO-SYMBOLS-INSERT-ARGS-AT-PLUS "the_point+units_from+towards+on_road?" '(d x y z))
; > (THE POINT D UNITS FROM X TOWARDS Y ON ROAD Z)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN SPLIT-STRING-TO-SYMBOLS-INSERT-ARGS-AT-PLUS (STRING ARG-LIST &OPTIONAL (SEPARATORS "-_+? "))
	(UNLESS (SIMPLE-STRING-P STRING)     (SETQ STRING     (COPY-SEQ STRING)))
	(UNLESS (SIMPLE-STRING-P SEPARATORS) (SETQ SEPARATORS (COPY-SEQ SEPARATORS)))
	(LET ( (CHUNKS  '()) (POSITION 0) (NEXTPOS  0) (STRLEN (LENGTH STRING)) )
		(DECLARE (TYPE SIMPLE-STRING STRING SEPARATORS))
		(LOOP WHILE (< POSITION STRLEN)
	   		DO
	   		(LOOP WHILE (AND (< NEXTPOS STRLEN) (NOT (POSITION (CHAR STRING NEXTPOS) SEPARATORS)))
	      		DO 
	      		(SETQ NEXTPOS (1+ NEXTPOS))
	      	)
	      	(when (> NEXTPOS POSITION)	      	
	   			(PUSH (INTERN (string-upcase (SUBSEQ STRING POSITION NEXTPOS) )) CHUNKS)
	   		)
	      	(when (AND (< NEXTPOS STRLEN) (eq (CHAR STRING NEXTPOS) '#\+))
	   			(MAPCAR #'(LAMBDA (ELEMENT) (PUSH ELEMENT CHUNKS)) (SPLIT-STRING-TO-SYMBOLS (STRING (pop ARG-LIST))))
	   		)   		
	   		(SETQ POSITION (1+ NEXTPOS))
	   		(SETQ NEXTPOS  POSITION)
	   	)
	   	(when (not (null ARG-LIST))
	   		(MAPCAR #'(LAMBDA (ELEMENT) (PUSH ELEMENT CHUNKS)) (SPLIT-STRING-TO-SYMBOLS (STRING (pop ARG-LIST))))
	   	)
		(NREVERSE CHUNKS)
	)
); end of SPLIT-STRING-TO-SYMBOLS-INSERT-ARGS-AT-PLUS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function SPLIT-STRING-TO-SYMBOLS takes a well-formed formula string as 
;; arg STRING and an optional arg of tokenizer symbols in double quotes.
;; The function translates STRING into an English sentence by removing 
;; spaces, and by replacing underscores and dashes and question marks with 
;; spaces. This function is called by function verbalize-wff and was 
;; modified from function SPLIT-STRING of the following source:
;; <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;; http://darcs.informatimago.com/darcs/public/lisp/common-lisp/string.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN SPLIT-STRING-TO-SYMBOLS (STRING &OPTIONAL (SEPARATORS "-_+? "))
"
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
	(UNLESS (SIMPLE-STRING-P STRING)     (SETQ STRING     (COPY-SEQ STRING)))
	(UNLESS (SIMPLE-STRING-P SEPARATORS) (SETQ SEPARATORS (COPY-SEQ SEPARATORS)))
	(LET ( (CHUNKS  '()) (POSITION 0) (NEXTPOS  0) (STRLEN (LENGTH STRING)) )
		(DECLARE (TYPE SIMPLE-STRING STRING SEPARATORS))
		(LOOP WHILE (< POSITION STRLEN)
	   		DO
	   		(LOOP WHILE (AND (< NEXTPOS STRLEN) (NOT (POSITION (CHAR STRING NEXTPOS) SEPARATORS)))
	      		DO (SETQ NEXTPOS (1+ NEXTPOS))
	      	)
	   		(when (> NEXTPOS POSITION) 
	   			(PUSH (INTERN (string-upcase (SUBSEQ STRING POSITION NEXTPOS) )) CHUNKS)
	   		)
	   		(SETQ POSITION (1+ NEXTPOS))
	   		(SETQ NEXTPOS  POSITION)
	   	)
		(NREVERSE CHUNKS)
	)
); end of SPLIT-STRING-TO-SYMBOLS

