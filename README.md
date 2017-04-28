# affectTED
### Exploring single-agent personality and emotions in the Gridworld architecture

Written by Santiago Loane and Nathaniel Potrepka

## The Basics

The architecture of our emotional agent, ```TED```, is twofold. First, we define a “personality” for the agent based on the Five Factor Model, which is a five-trait model of personality often given the acronym OCEAN for Openness, Conscientiousness, Extraversion, Agreeableness, and Neuroticism. Each personality trait is in the range ```[-1, 1]```. Second, we define just one dimension of emotion for the agent, positive/negative valence, within a range of ```[-5, 5]```. Given more time, we would implement high/low arousal and possibly open/closed stance.

## Implementation

The personality traits for ```TED``` are first defined in the hash table ```*traits*```, defined in ```gridworld-definitions.lisp```. Later on in our world, ```our-world.lisp```, we use the values of this hash table to set facts for ```TED```, ```is_open_to_degree```, ```is_conscientious_to_degree```, ```is_extroverted_to_degree```, etc., so that the personality traits can be later referenced by actions. A fact is also given to ```TED``` concerning his emotional valence, namely ```is_happy_to_degree```.

Many actions were then rewritten (or newly written) to depend on the personality traits and to influence the emotional state. For example,
```lisp
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
```
becomes
```lisp
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
```

A full list of actions for ```TED``` is as follows:
```
answer_user_ynq ;for utility purposes, these two
answer_user_whq ;are not dependent on personality
die
walk
eat
sleep
drink
ask+whether
play
play+with
talk+with
heal
```

Additionally, we have added the possibility of death. Death is implemented as an exogenous event, which required a bit of extra effort to get working right, but essentially, for parameters ```(?u ?h ?f)```, the action ```die``` has preconditions
```lisp
(is_alive TED)
(is_tired_to_degree TED ?f)
(is_thirsty_to_degree TED ?h)
(is_hungry_to_degree AH ?u)
(>= (+ ?f ?h ?u) 30.0)
```
and effects
```lisp
(is_dead TED)
(not (is_alive TED))
```
In the agent's version of ```die``` (as opposed to the ```actual``` version), a very low value is given to ```die```, specifically ```-999```, in order to ensure that ```TED``` avoids death at all costs.

## Compile & Run

To compile and run the code, call:
```
> (load "init")
```

and to run a step of the agent, simply call:
```
> (go!)
```

Additionally, some question answering is possible:
```
> (listen!)
((ask-yn (TED is_bored))
 (ask-wh (TED is_happy_to_degree ?h)))
```

It is also possible to run some basic tests using:
```
> (load "test")
```

## References

Liu, D. (2011). Gridworld Framework Manual. Retrieved April 28, 2017, from [https://www.cs.rochester.edu/~schubert/191-291/gridworld-code/Manual/GridworldManual.pdf](https://www.cs.rochester.edu/~schubert/191-291/gridworld-code/Manual/GridworldManual.pdf)
