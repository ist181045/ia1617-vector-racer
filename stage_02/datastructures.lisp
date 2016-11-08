
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data structures for the project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; List of possible actions
(defun possible-actions ()
  '((0 0) (0 1) (1 0) (-1 0) (0 -1) (1 -1) (-1 1) (1 1) (-1 -1)))

;;; Definition of track
;;;  * size - size of track
;;;  * env - track where nil are obstacles, everything else is the track
;;;  * startpos - initial position
;;;  * endpositions - list of valid final positions
(defstruct track 
  size 
  env 
  startpos 
  endpositions)

;;; State of the car
;;;  * pos - position
;;;  * vel - velocity
;;;  * action - action that was used to generate the state
;;;  * cost - cost of the action
;;;  * track - VectorRace track
;;;  * other - additional information
(defstruct state 
  pos 
  vel
  action
  cost
  track
  other)
						
;;; Definition of a problem
;;;  * initial-state
;;;  * fn-nextstate - function that computes the successors of a state
;;;  * fn-isGoal - funtion that identifies a goal state
;;;  * fn-h - heuristic function
(defstruct problem 
  initial-state
  fn-nextStates 
  fn-isGoal 
  fn-h)

;;; Definition of a search node
;;;  * parent - the parent node
;;;  * state - the state of the node
;;;  * f - estimate of the cost
;;;  * g - cost from the initial node to the current node
;;;  * h - estimate of the cost from the current node to the nearest goal
(defstruct node
  parent
  state
  f
  g
  h)
