(load "datastructures.fas")
(load "auxfuncs.fas")


;;; TAI position
(defun make-pos (l c)
  (list l c))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (l c)
  (list l c))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (l c)
  (list l c))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))

;;; order list of coordinates by column then row ascending
(defun ordercoordinates (coordlist)
  ;; order list of coordinates
  (stable-sort (copy-alist coordlist)
    #'(lambda (x y)
      (< (+ (* (car x) 10) (second x))
         (+ (* (car y) 10) (second y))))))


;; Solution of phase 1

;;; getTrackContent Helper
(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; isObstaclep
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; isGoalp
(defun isGoalp (st)
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
        (track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp) T)))

;; nextState
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
    (make-vel (+ (vel-l (state-vel st)) (acce-l act))
              (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
    (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
              (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
    (cond ((isGoalp new-state) -100)
          ((isObstaclep (state-pos new-state) (state-track new-state)) 20)
          (T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
                                            (pos-c (state-pos st)))))
    (values new-state)))



;; Solution of phase 2

;;; nextStates
(defun nextStates (st)
  (loop for act in (reverse (possible-actions))
    collect (nextState st act)))

;;; limdepthfirstsearch
(defun limdepthfirstsearch (problem lim)
  (let* ((firstNode (make-node :state (problem-initial-state problem)))
         (result (recursiveDFS firstNode problem lim)))
    (if (node-p result) (generateSolution result) result)))

(defun generateSolution (node)
  (if (null node) ()
      (nconc (generateSolution (node-parent node)) (list (node-state node)))))

(defun recursiveDFS (node problem lim)
  (cond ((funcall (problem-fn-isGoal problem) (node-state node)) node)
        ((zerop lim) :corte)
        (t (let ((cutoff? nil))
             (dolist (s (funcall (problem-fn-nextStates problem) (node-state node)))
               (let* ((child (make-node :parent node :state s))
                      (result (recursiveDFS child problem (1- lim))))
                 (cond ((eq result :corte) (setf cutoff? t))
                       ((not (null result)) (return-from recursiveDFS result)))))
               (if cutoff? :corte nil)))))


;;; iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
  (dotimes (depth lim)
    (let ((result (limdepthfirstsearch problem depth)))
      (when (listp result) (return result)))))



;;; Solution to phase 3

;;; compute-heuristic
(defun compute-heuristic (st)
  (cond ((isGoalp st) 0)
    (T (let* ((col (pos-c (first (ordercoordinates (track-endpositions (state-track st))))))
              (dist (abs (- col (pos-c (state-pos st))))))
         (if (isObstaclep (state-pos st) (state-track st)) most-positive-fixnum dist)))))



;;; A* search
(defun a* (problem)
  (problem-intial-state problem))
