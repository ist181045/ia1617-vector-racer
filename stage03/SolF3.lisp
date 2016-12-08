(load "datastructures.lisp")
(load "auxfuncs.lisp")

;;; Abstract data types

;; Position
;; make-pos: l x c -> position (list)
(defun make-pos (r c)
  (list r c))
;; pos-r: pos -> number (row)
(defun pos-r (pos)
  (first pos))
;; pos-c: pos -> number (column)
(defun pos-c (pos)
  (second pos))

;; Acceleration
;; make-acce: l x c -> acceleration (list)
(defun make-acce (r c)
  (list r c))
;; acce-r: acce -> number (row)
(defun acce-r (acce)
  (first acce))
;; acce-c: acce -> number (column)
(defun acce-c (acce)
  (second acce))

;; Velocity
;; make-vel: l x c -> velocity (list)
(defun make-vel (r c)
  (list r c))
;; vel-r: vel -> number (row)
(defun vel-r (vel)
  (first vel))
;; vel-c: vel -> number (column)
(defun vel-c (vel)
  (second vel))


;;; Solution of phase 1

;; getTrackContent: pos x track -> element
;; returns the element in the given position of the given track. Could be:
;; -   T: not an obstacle
;; - NIL: obstacle
(defun getTrackContent (pos track)
  "return the element in the given position"
  (nth (pos-c pos) (nth (pos-r pos) (track-env track))))

;; isObstaclep: pos x track -> boolean
;; returns T if the given pos is outside of the track or element is NIL,
;    NIL otherwise
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-r pos) 0) (< (pos-c pos) 0)
      (>= (pos-r pos) (pos-r (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; isGoalp: state -> boolean
;; returns T if state's position is in the track's endpositions, NIL otherwise
(defun isGoalp (st)
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
        (track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp) T)))

;; nextState: state x action -> state
;; returns the state after applying action 'act' to state 'st'
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
    (make-vel (+ (vel-r (state-vel st)) (acce-r act))
              (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
    (make-pos (+ (pos-r (state-pos st)) (vel-r (state-vel new-state)))
              (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
    (cond ((isGoalp new-state) -100)
          ((isObstaclep (state-pos new-state) (state-track new-state)) 20)
          (T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-r (state-pos st))
                                            (pos-c (state-pos st)))))
    (values new-state)))



;;; Solution of phase 2

;; nextStates: state -> list
;; returns a list of every state after applying every possible action to st
(defun nextStates (st)
  "returns a list of states after applying every possible action to st"
  (loop for act in (reverse (possible-actions))
    collect (nextState st act)))

;; limdepthfirstsearch: problem x lim -> node?
;; performs a recursive depth first search up until the limit is reached for a
;; solution to the given problem, return the node with the goal state or :corte
(defun limdepthfirstsearch (problem &optional (lim most-positive-fixnum))
  "performs a limited dfs and returns either the solution or :corte"
  (let ((firstNode (make-node :state (problem-initial-state problem)))
        (result))
    (labels ((recursiveDFS (node problem &optional (lim most-positive-fixnum))
      (cond ((funcall (problem-fn-isGoal problem) (node-state node)) node)
            ((zerop lim) :corte)
            (t (let ((cutoff? nil))
                 (dolist (s (funcall (problem-fn-nextStates problem) (node-state node)))
                   (let* ((child (make-node :parent node :state s))
                          (result (recursiveDFS child problem (1- lim))))
                     (cond ((eq result :corte) (setf cutoff? t))
                           ((not (null result)) (return-from recursiveDFS result)))))
                   (if cutoff? :corte nil))))))
    (setf result (recursiveDFS firstNode problem lim)))
    (if (node-p result) (solution result) result)))

;; iterlimdepthfirstsearch: problem x lim -> list?
;; iteratively execute a limited depth first search for a solution to the problem
;; using limdepthfirstsearch. See limdepthfirstsearch for returned value
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
  "iteratively execute a limited depth first search"
  (dotimes (depth lim)
    (let ((result (limdepthfirstsearch problem depth)))
      (when (listp result) (return result)))))

;; solution: node -> list
;; builds and returns a list of a path to the goal state by traversing the
;; last node with said state's parents recursively
(defun solution (node)
  "builds and returns a list of a path to the goal state"
  (if (null node) ()
    (nconc (solution (node-parent node)) (list (node-state node)))))



;;; Solution to phase 3

;; compute-heuristc: state -> number
;; calculates the distance on the x-axis (column difference) from the current
;; position to the closest end position
(defun compute-heuristic (st)
  "returns one-dimension distance between current and closest end position"
  (cond ((isGoalp st) 0)
        ((isObstaclep (state-pos st) (state-track st)) most-positive-fixnum)
        (t (let ((dist most-positive-fixnum))
            (dolist (pos (track-endpositions (state-track st)))
              (let ((attempt (abs (- (pos-c pos) (pos-c (state-pos st))))))
                (if (< attempt dist) (setf dist attempt))))
            dist))))

;; manhattan-distance: state -> number
;; calculates the manhattan distance between the current position and the
;; closest end position (abs(xf - x0) + abs(yf - y0))
(defun manhattan-distance (st)
  "calculates the manhattan distance between current and closest end positions"
  (cond ((isGoalp st) 0)
        ((isObstaclep (state-pos st) (state-track st)) most-positive-fixnum)
        (t (let ((dist most-positive-fixnum))
            (dolist (a (track-endpositions (state-track st)))
              (let* ((dx (abs (- (pos-c a) (pos-c (state-pos st)))))
                     (dy (abs (- (pos-r a) (pos-r (state-pos st)))))
                     (d (+ dx dy)))
                  (if (< d dist) (setf dist d))))
            dist))))

;; insert-sorted: list x node x predicate -> list
;; takes a node and inserts it in the list according to the given predicate
(defun insert-sorted (lst node &key (compare #'<=))
  "inserts a node in the given list using the comparing function"
  (cond ((null lst) (list node))
        ((funcall compare (node-f node) (node-f (car lst))) (push node lst))
        (t (append (list (car lst)) (insert-sorted (rest lst) node :compare compare)))))


;; a*: problem -> list?
;; performs an A* guided search using the problem's heuristic function. returns
;; a list with the path to the goal state, nil otherwise
(defun a* (problem)
  "performs an A* guided search, returning the path to the solution"
  (let ((open (list)) (closed (list)))
    (push (make-node :state (problem-initial-state problem)
                     :g (state-cost (problem-initial-state problem))) open)

    (loop do
      (if (null open) (return-from a* nil))
      (let ((curr (pop open)))
        (if (funcall (problem-fn-isGoal problem) (node-state curr))
          (return-from a* (solution curr)))
        (push curr closed)
        (loop for st in (funcall (problem-fn-nextStates problem) (node-state curr)) do
          (let ((nodeInOpen) (nodeInClosed)
               (new (make-node :state st :parent curr
                               :g (+ (node-g curr) (state-cost st))
                               :h (funcall (problem-fn-h problem) st))))
            (setf (node-f new) (+ (node-g new) (node-h new)))
            (setf nodeInOpen (car (member (node-state new) open :test #'equalp :key #'node-state)))
            (setf nodeInClosed (car (member (node-state new) closed :test #'equalp :key #'node-state)))
            (cond ((and (null nodeInOpen) (null nodeInClosed))
                    (setf open (insert-sorted open new)))
                  ((and (not (null nodeInOpen)) (< (node-f new) (node-f nodeInOpen)))
                    (substitute new nodeInOpen open :test #'equalp)))))))
    nil))

;; best-search: problem -> list?
;; performs an a* search using a different heuristic than the default provided
;; by the problem
(defun best-search (problem)
  "performs an a* search using the manhattan distance heuristic"
  (let ((new-problem (copy-problem problem)))
    (setf (problem-fn-h new-problem) #'manhattan-distance)
    (a* new-problem)))
