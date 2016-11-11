
;;; this is your solution file
(load "SolF2.lisp")

(defun states-to-list (stts)
  (loop for st in stts
	  collect
	  (list	  (state-pos st)
		  (state-vel st)
		  (state-action st)
		  (state-cost st))))

(defun initial-state (track)
  (make-state :pos (track-startpos track)
	      :vel (make-vel 0 0) 
	      :action nil
	      :cost 0
	      :track track))

		  
(defvar *track*)
(setf *track* (loadtrack "track1.txt"))

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

(print "Exercise 2.1 - nextStates")
(with-open-file (str "out2.1.txt"
		 :direction :input)
  (format t "~% Solution is correct? ~a" (equal (states-to-list (nextStates non-goal-state)) (read str))))

(print "Exercise 2.1b - nextStates")
(with-open-file (str "out2.1b.txt"
		 :direction :input)
  (format t "~% Solution is correct? ~a" (equal (states-to-list (nextStates prev-goal-state)) (read str))))
  
(setf *p1* (make-problem :initial-state (initial-state *track*)
						 :fn-isGoal #'isGoalp
						 :fn-nextstates #'nextStates))

			 
(print "Exercise 2.2 - limdepthfirstsearch")
(let ((real1 (get-internal-real-time)))

		(with-open-file (str "out2.2.txt"
			 :direction :input)
	  (format t "~% Solution is correct? ~a" (equal (states-to-list  (limdepthfirstsearch *p1* 6)) (read str))))
	
    (let ((real2 (get-internal-real-time)))
	(format t "~%Computation took: ~f seconds of real time~%"
		(/ (- real2 real1) internal-time-units-per-second))))
  

(print "Exercise 2.3 - iterlimdepthfirstsearch")
(let ((real1 (get-internal-real-time)))

	(with-open-file (str "out2.3.txt"
			 :direction :input)
	  (format t "~% Solution is correct? ~a" (equal (states-to-list  (iterlimdepthfirstsearch *p1*)) (read str))))
	
    (let ((real2 (get-internal-real-time)))
	(format t "~%Computation took: ~f seconds of real time~%"
		(/ (- real2 real1) internal-time-units-per-second))))						 

		
(setf (track-startpos *track*) '(2 15))
(setf *p1* (make-problem :initial-state (initial-state *track*)
						 :fn-isGoal #'isGoalp
						 :fn-nextstates #'nextStates))  
						 
(print "Exercise 2.2b - limdepthfirstsearch")
(let ((real1 (get-internal-real-time)))

		(with-open-file (str "out2.2b.txt"
			 :direction :input)
	  (format t "~% Solution is correct? ~a" (equal (states-to-list  (limdepthfirstsearch *p1* 6)) (read str))))
	
    (let ((real2 (get-internal-real-time)))
	(format t "~%Computation took: ~f seconds of real time~%"
		(/ (- real2 real1) internal-time-units-per-second))))
  

(print "Exercise 2.3b - iterlimdepthfirstsearch")
(let ((real1 (get-internal-real-time)))

	(with-open-file (str "out2.3b.txt"
			 :direction :input)
	  (format t "~% Solution is correct? ~a" (equal (states-to-list  (iterlimdepthfirstsearch *p1*)) (read str))))
	
    (let ((real2 (get-internal-real-time)))
	(format t "~%Computation took: ~f seconds of real time~%"
		(/ (- real2 real1) internal-time-units-per-second))))	    
 