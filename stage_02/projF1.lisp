
;;; this is your solution file
(load "SolF1.lisp")

(defun state-to-str (st)
  (format nil "Pos:~a Vel:~a Action:~a Cost:~a" 
	  (state-pos st)
	  (state-vel st)
	  (state-action st)
	  (state-cost st)))

(defvar *track*)
(setf *track* (loadtrack "track0.txt"))

(defvar st)
(setf st (make-state :pos (track-startpos *track*)
		     :vel '(0 1) 
		     :action nil
		     :cost 0
		     :track *track*))
	  
(defvar action)
(setf action '(1 -1))

(defvar prev-goal-state)
(setf prev-goal-state 
  (make-STATE :POS '(2 13)
	      :VEL '(0 4)
	      :ACTION '(1 1)
	      :COST 1
	      :TRACK *track*
	      :OTHER NIL))

(defvar goal-state)
(setf goal-state 
  (make-STATE :POS '(3 16)
	      :VEL '(1 3)
	      :ACTION '(1 -1)
	      :COST -100
	      :TRACK *track*
	      :OTHER NIL))


(defvar non-goal-state)
(setf non-goal-state 
  (make-STATE :POS '(3 6)
	      :VEL '(-1 2)
	      :ACTION '(-1 0)
	      :COST 1
	      :TRACK *track*
	      :OTHER NIL))

(defvar obstacle)
(setf obstacle '(2 2))
    
(defvar non-obstacle)
(setf non-obstacle '(2 8))

;;; ===================================================================================================
(print "Exercise 1.1 - isObstaclep")

(with-open-file (str "out1.1a.txt"
		 :direction :input)
  (format t "~% Solution is correct? ~a" (equalp (format nil "~a" (isObstaclep obstacle *track*)) (read str))))

(with-open-file (str "out1.1b.txt"
		 :direction :input)
  (format t "~% Solution is correct? ~a" (equalp (format nil "~a" (isObstaclep non-obstacle *track*)) (read str))))

;;; ===================================================================================================
(print "Exercise 1.2 - isGoalp")

(with-open-file (str "out1.2a.txt"
		 :direction :input)
  (format t "~% Solution is correct? ~a" (string= (format nil "~a" (isGoalp goal-state)) (read str))))

(with-open-file (str "out1.2b.txt"
		 :direction :input)
  (format t "~% Solution is correct? ~a" (string= (format nil "~a" (isGoalp non-goal-state)) (read str))))

;;; ===================================================================================================
(print "Exercise 1.3 - nextState")

(with-open-file (str "out1.3a.txt"
		 :direction :input)
  (format t "~% Solution is correct? ~a" (string= (state-to-str (nextState prev-goal-state action)) (read str))))

(with-open-file (str "out1.3b.txt"
		 :direction :input)
  (format t "~% Solution is correct? ~a" (string= (state-to-str (nextState st action)) (read str))))


;;; ===================================================================================================

