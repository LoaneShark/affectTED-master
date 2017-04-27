;;; TEST FILE

(load "gridworld-definitions.lisp")
(load "gridworld-planning.lisp")
(load "go.lisp")
(load "implement-effects.lisp")
(load "simulation-and-function-evaluation.lisp")

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

;; Default values
(setf (gethash 'O *traits*) 0.0)
(setf (gethash 'C *traits*) 0.0)
(setf (gethash 'E *traits*) 0.0)
(setf (gethash 'A *traits*) 0.0)
(setf (gethash 'N *traits*) 0.0)
(load "our-world.lisp")
(initialize-state-node)
(go!)
