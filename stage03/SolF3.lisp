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
;; calculates the heuristic value of a given state's position. If the value is
;; initially unknown (i.e. other's car isn't the same track as the state's), it
;; computes the heuristic value for every single position and stores it in the
;; state.
;;
;; !!WARNING!! It modifies the state! Children of this state should bear the same
;; value of the parent's other attribute.
(defun compute-heuristic (st)
  "returns the heuristic value of a position, if exists. Else, BFS to plan out all"
  (cond ((or (null (state-other st)) (not (eq (state-track st) (car (state-other st)))))
    (let* ((rows (car (track-size (state-track st))))
           (cols (cadr (track-size (state-track st))))
           (hmat (make-array (list rows cols) :initial-element most-positive-fixnum)))
      (dolist (endpos (track-endpositions (state-track st)))
        (setf (aref hmat (pos-r endpos) (pos-c endpos)) 0))

        (let ((curr) (explored) (adjh) (candh)
              (queue (list (first (track-endpositions (state-track st))))))
        (loop while (not (null queue)) do
          (setf curr (pop queue))
          (setf candh (aref hmat (pos-r curr) (pos-c curr)))
          (dolist (next (possible-actions))
            (setf next (mapcar #'+ next curr))
            (cond ((and (not (member curr explored :test #'equal))
                (not (isObstaclep next (state-track st))))
              (setf adjh (min (aref hmat (pos-r next) (pos-c next)) (1+ candh)))
              (setf (aref hmat (pos-r next) (pos-c next)) adjh)
              (push next queue))))
          (push curr explored)))
    (setf (state-other st) (list (state-track st) hmat)))))
  (aref (cadr (state-other st)) (pos-r (state-pos st)) (pos-c (state-pos st))))

;; insert-sorted: list x node x predicate -> list
;; takes a node and inserts it in the list according to the given comparator
(defun insert-sorted (node seq &key (compare #'<=))
  "inserts a node in the given list using the comparing function"
  (cond ((null seq) (list node))
        ((funcall compare (node-f node) (node-f (car seq))) (push node seq))
        (t (append (list (car seq)) (insert-sorted node (rest seq) :compare compare)))))

;; cmp-node-pos: node x node -> generalized-boolean
;; returns true if the position and velocity of the nodes' states are the same
(defun cmp-node-stts (node other)
  "checks if the nodes' states' positions are the same"
  (and (equal (state-pos (node-state node)) (state-pos (node-state other)))
       (equal (state-vel (node-state node)) (state-vel (node-state other)))))

;; a*: problem -> list?
;; performs an A* guided search using the problem's heuristic function. returns
;; a list with the path to the goal state, nil otherwise
(defun a* (problem)
  "performs an A* guided search, returning the path to the solution"
  (let ((open (list)) (closed (list)))
    (push (make-node :state (problem-initial-state problem)
                     :g (state-cost (problem-initial-state problem))
                     :h (funcall (problem-fn-h problem) (problem-initial-state problem))) open)

    (loop do
      (if (null open) (return-from a* nil))
      (let ((curr (pop open)))
        (if (funcall (problem-fn-isGoal problem) (node-state curr))
          (return-from a* (solution curr)))
        (push curr closed)
        (loop for st in (funcall (problem-fn-nextStates problem) (node-state curr)) do
          (let ((nodeInOpen (car (member (make-node :state st) open :test #'cmp-node-stts)))
                (nodeInClosed (car (member (make-node :state st) closed :test #'cmp-node-stts)))
                (new (make-node :state st
                                :parent curr
                                :g (+ (node-g curr) (state-cost st)))))
            (setf (state-other (node-state new)) (state-other (node-state curr)))
            (setf (node-h new) (funcall (problem-fn-h problem) st))
            (setf (node-f new) (+ (node-g new) (node-h new)))
            (cond ((and (null nodeInOpen) (null nodeInClosed))
                    (setf open (insert-sorted new open)))
                  ((and (not (null nodeInOpen)) (< (node-f new) (node-f nodeInOpen)))
                    (setf open (remove nodeInOpen open :test #'eq))
                    (setf open (insert-sorted new open))))))))
    nil))

;; best-search: problem -> list?
;; performs an a* search. A* was already optimized, including a list for closed,
;; nodes, as was the heuristic's computation, which is done only once per
;; different search.
(defun best-search (problem)
  "performs an a* search"
  (a* problem))
