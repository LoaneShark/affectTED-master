
This README file is intended as a quick initial overview of the
steps needed to run Gridworld and create new worlds (or as a 
reminder to those already acquainted with the system). See 
also the descriptive overview in "informal-overview.txt"
(available both in this directory and the "Manual" subdirectory).

NOTE (especially to CSC 291 students): 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Reading this file is not an alternative to consulting the manual 
(in the "Manual" subdirectory). The manual (at 19pp not a lengthy 
document) is essential for fully understanding the Gridworld 
environment and its modification and use.

Running the sample world
~~~~~~~~~~~~~~~~~~~~~~~~
File "gridworld-world.lisp" contains the definition of the sample 
world.

If you just want to run this sample world, you can do
  (load "init.lisp"),
and then you can say (go!) to have the agent choose and take one 
action. You can also convey a question or fact to the agent by
using (listen!). You can then input a singleton list containing
a question or fact, or a longer list of questions or facts e.g., 
(note the infixing of predicates)
  ((ask-yn (pizza3 is_edible))),
or
  ((ask-wh (ME is_tired_to_degree ?x))
   (tell (guru knows (whether (juice3 is_potable)))));
the agent will immediately store facts (if consistent with the world), 
and for the questions will store the fact that the user wants an 
answer to the question; on the next (go!) it will decide whether 
to answer right away or to do something more urgent (in practice, 
questions are usually answered right away, if the "reward" for 
answering has been made high enough).

Creating new worlds
~~~~~~~~~~~~~~~~~~~
But what you really need to know is how to create your own
agent and world. In summary, what you do is the following (unless
mentioned otherwise, these specifications are entered into the
file "gridworld-world.lisp"; some changes to "gridworld-definitions"
are also needed):

1. Define a set of points (places) and paths; this is done with
   a 'def-roadmap' function; e.g., 
     (def-roadmap '(home grove plaza) 
                  '((path1 home 3 grove) (path2 home 2 plaza)))
   defines 3 points (home, grove, and plaza) and a path of length 3
   connecting 'home' with 'grove' and a path of length 2 connecting 
   'home' with 'plaza'. (Paths can have multiple steps, e.g., we
   could have used '((loop1 home 3 grove 4 plaza 2 home)) as path.)

   These definitions are automatically expanded to add certain
   facts to *roadmap-knowledge*, namely, each path is predicated
   to be a 'road' (e.g., (road path1)), each point is predicated
   to be on whichever paths it has been specified with (e.g.,
   (is_on grove path1)), and each path is predicated to be navigable
   (e.g., (navigable path1)). However, navigability might be
   changed by certain operators, like the occurrence of fire.

2. Define various object types and/or agent types; this is done 
   with a 'def-object' function; e.g.,
      (def-object 'robot '(is_animate can_talk))
      (def-object 'juice '(is_inanimate is_potable (has_cost 2.0)));
   This says that a robot is a type of object is animate and can talk,
   and juice is a type of thing that is inanimate, potable, and 
   costs 2 units. Corresponding explicit general predicate formulas 
   (e.g., (((robot ?x)) => (can_talk ?x)) are added to the agent's 
   knowledge and the specification of the actual world.

3. Create various specific objects (of the defined types) at various
   points, also specifying current (i.e., initial) possessions and 
   facts about the objects; this is done using the 'place-object' 
   function; e.g.,
       (place-object 'AG 'robot 'home 0
        nil ; an empty list of "possessions" (associated things)
        '((is_hungry_to_degree AG 4.0)
              (is_thirsty_to_degree AG 2.0)
          (is_tired_to_degree AG 0.0))
        '((knows AG (whether (is_edible pizza3)))))
   This creates a robot 'AG', places it at 'home', sets the start 
   time to 0, provides an empty list of things associated with AG
   (associated things can be things like money in the case of an
   agent, fruits in the case of a fruit tree, etc.), provides 
   current simple facts about AG, and also a list (here just a
   singleton) of propositional attitudes (knowing and wanting)
   held by AG. In the case of the main agent, here AG, we can 
   also include miscellaneous other facts about other things and
   places (not necessarily involving AG) among the current facts,
   which will ensure that these are placed in AG's model of the
   world.

   Again explicit formulas are created as appropriate (e.g.,
   (is_at AG home), and (has AG ...) formulas for AG's possessions).

4. Specify the occluded predicates by replacing the value of the global 
   variable *occluded-preds* with your list of occluded predicates (ones
   whose truth or falsity is not "obvious at a glance" to the agent,
   such as 'contains' for closed containers, or 'knows' and 'likes'
   for other agents), via the line (defvar *occluded-preds* ...) in
   file "gridworld-definitions.lisp". See page 9 of the manual.

   You may also want to specify *left-comoving-preds* and 
   *right-comoving-preds*, in "gridworld-definitions.lisp".
   For example, if 'has' is specified as right-comoving, this
   means that if, say, we have (has AG banana1), then if the 
   agent moves to another location, this predication will still
   be true. (Note that 'banana1' is the righ-hand argument of 
   'has'.) If 'is_in' is left-comoving, then if for instance
   (is_in key1 box1), and we have (has AG box1), then the key 
   will move with the box, and thus with the agent.

   NOTE: "gridworld-definitions.lisp" also contains a specific list
    of *epistemic-preds* (a superset of those in the sample world).
    This list should probably kept as-is, unless you are prepared 
    to modify the Gridworld inference routines, which currently 
    handle only attitudes of type (knows <...> (that <...>)),
    (knows <...> (whether <...>)), and (wants <...> (that <...>)).

5. Optionally add miscellaneous conditional (if-then) statements
   to the agent's *general-knowledge*; e.g., if we wanted to say
   that whenever there is a fire, every road is unnavigable, we
   could execute
     (push (list '((there_is_a_fire) (?x road)) '=> 
                                          '(not (?x navigable)))
      *general-knowledge*)
   Such general knowledge is used both in augmenting the agent's
   knowledge about the world states that it considers, and the 
   contents of simulated world states. The method is to instantiate
   the antecedent (preceding the '=>') in all possible ways and
   then to assert the corresponding consequents (after the '=>') as
   additional facts about a state. This method can be iterated,
   using newly inferred facts to derive additional ones. Currently
   the *inference-limit* in "gridworld-planning.lisp" is set to 2,
   limiting forward inference to 2 steps. Details can be found on
   p. 12 of the manual. Note that because of the way general if-then
   facts are used here for forward inference only, we can think of 
   them as "rules", and the manual refers to them in this way.
   (Be aware, though, that in general the concept of a general
   *fact* must be kept distinct from the concept of an inference
   *rule* in knowledge representation and reasoning; the former is
   simply a static symbolic object that is either true or false,
   while the latter specifies an *action*, intended to derive a
   new truth from known truths.)

   In the sample world, fire does in fact make roads unnavigable,
   but instead of being handled through a general conditional fact,
   it is handled by making unnavigability of roads an effect of
   the 'fire.actual' operator. There is in general a trade-off
   between handling "cascaded" effects of change in the world by
   specifying all such effects in the operators, and handling
   them by forward inference using world knowledge -- thus keeping
   the operators simpler (because they only need to specify primary
   effects, not cascaded effects).
      
6. Define a list of names of operators (action types) that you want
   to use; e.g.,
     (setq *operators* '(walk eat answer_user_ynq answer_user_whq 
                          sleep drink ask+whether play));

7. Define a "search beam" to limit how many steps into the future
   the agent should search in deciding on actions, and for each
   search depth, how many alternatives, selected from what operators,
   you want it to consider at that depth; e.g.,
     (setq *search-beam*
       (list (cons 5 *operators*) (cons 4 *operators*) 
             (cons 3 *operators*) ));
   This limits search to depth 3, and allows fan-outs of 5, 4, and
   3 for the first, second, and third steps of the search, in this 
   case always allowing the choice of alternatives to be made from
   all operators in the *operators* list. In general, one has to 
   experiment with search beams, restricting them sufficiently 
   so that the search will not be excessively time-consuming.

8. Provide two operator definitions for each operator under the
   agent's control. The first is the agent's model (understanding)
   of the operator, i.e., what it will use for planning (using a 
   forward search restricted by the search beam). The second definition,
   which uses the same name for the operator *except* that it is 
   followed by extension ".actual", is the definition used in the
   simulation of the world -- whose behavior in general won't 
   completely match the agent's model-based expectations, because
   an action may, for example, be interrupted unexpectedly by a fire.

   The external ("exogenous") events not under the agent's control
   are each defined by just one operator, with an ".actual"
   extension. (The sample world uses 'rain.actual' and 'fire.actual'.)
   The actual simulation of the agent's actions, and exogenous events,
   is generally done in (potentially) multiple steps per action, 
   rather than just one step per action. In particular, action
   definitions have a 'time-required' field, and the simulation will
   be broken into n steps if the time required is specified to be n.

   Action operators as "seen" by the agent have 'preconds'
   (preconditions) and 'effects', plus the time-required field and
   a 'value' field. Preconds and effects are logical formulas
   stating what must be true for the operator to be executable,
   and the expected effect of executing it. The 'value' field is
   very important: this is in part what *motivates* the agent, 
   i.e., how much it "enjoys" or "dislikes" carrying out the
   action (more on this below).

   Preconditions, effects, times required and values can all be 
   specified using functional formulas, where input parameters may
   be used in those formulas. For seeing detailed examples, it's 
   best to go to the sample world in "gridworld-world.lisp", and 
   of course to the further explanations in the manual. But an 
   important point is that this functional capability even allows
   for question-answering, by specifying an effect for an operator
   such as 'answer_user_whq' that depends on computing an answer
   to the user's question (where that computation will look into 
   the agent's world model to obtain an answer).

   Actual operators have a slightly different syntax from the model 
   operators, to allow for interruptions. In particular, they have
   'startconds' -- conditions under which the action can *actually*
   start (e.g., no interference from fire), and conditions that
   will stop it, which include conditions under which the action
   is successfully completed, but may also include other, interfering
   conditions. They also have 'deletes' and 'adds' fields, where the
   exact conditions are specified when the action is stopped --
   possibly before it has been completed (e.g., being brought to
   a stop in a 'walk' action from one place to another, when a fire
   breaks out).

9. Specify the values (rewards/costs) of the various actions (as 
   already mentioned in (8) above), and of potential states reached, 
   as follows (see pp. 7-8 of the manual):

   - First, prescribe the anticipated net utility of each model 
     operator that you design and use in your Gridworld by setting
     the ':value' field of each model operator appropriately; this 
     value could either be a static numerical measure or be dependent
     on (i.e., be a function of) the specific input arguments that 
     would be substituted into the formal parameters in instantiating 
     the model operator into a contemplated action.

   - Second, for the Gridworld that you design, assign values to
     specific properties that could be found in a state by modifying
     the contents of LISP routine (defun state-value (additions
     deletions prior-local-value) ...) in file "gridworld-planning.lisp". 
     This function essentially allocates positive points for addition of
     desirable properties to a state, and subtracts away these points for
     deletions of those properties; analogously for addition of 
     undesirable properties to, and removal of undesirable properties 
     from a See page 7 and top of page 8 of the manual.

10. Provide Lisp functions for the Lisp-evaluable functions (those
    with names ending in a question mark) used in any operators you
    have defined.
