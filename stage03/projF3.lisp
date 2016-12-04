
;;; this is your solution file
(load "SolF3.lisp")

(defun states-to-list (stts)
  (loop for st in stts
	  collect (format nil "POS: ~a VEL: ~a ACT: ~a COST: ~a~&"
	  (state-pos st)  (state-vel st)  (state-action st)  (state-cost st))))

(defun initial-state (track)
  (make-state :pos (track-startpos track) :vel (make-vel 0 0) :action nil :cost 0 :track track))

(defvar *t1* nil)
(defvar *p1* nil)

(setf *t1* (loadtrack "track0.txt"))

(format t "~&Exercise 3.1 - Heuristic~&")
(with-open-file (str "out3.1.txt"
		 :direction :input)
  (format t "~% Solution is correct? ~a~&" (equal (list (compute-heuristic (initial-state *t1*)) (compute-heuristic (make-state :pos '(1 6)  :track track)) (compute-heuristic (make-state :pos '(2 8)  :track track))) (read str))))
  
(setf *p1* (make-problem :initial-state (initial-state *t1*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'compute-heuristic))
			  
(format t "~&Exercise 3.2 - A*~&")
 (let ((real1 (get-internal-real-time)))
		 (with-open-file (str "out3.2.txt" :direction :input)
	   (format t "~% Solution is correct? ~a~&" (string= (format nil "~{~a~^~}" (states-to-list (a* *p1*))) (read str))))
      (let ((real2 (get-internal-real-time)))
	  (format t "~%Computation took: ~f seconds of real time~%" (/ (- real2 real1) internal-time-units-per-second))))
		
(defvar *t2* nil)
(defvar *p2* nil)

(setf *t2* (loadtrack "track9.txt"))

(format t "~&Exercise 3.1b - Heuristic~&")
(with-open-file (str "out3.1b.txt"
		 :direction :input)
  (format t "~% Solution is correct? ~a~&" (equal (list (compute-heuristic (initial-state *t2*)) (compute-heuristic (make-state :pos '(3 6)  :track track )) (compute-heuristic (make-state :pos '(3 8)  :track track ))) (read str))))
 
 
(setf *p2* (make-problem :initial-state (initial-state *t2*)  :fn-isGoal #'isGoalp	  :fn-nextstates #'nextStates	  :fn-h #'compute-heuristic))
			  
(format t "~&Exercise 3.2b - A*~&")
 (let ((real1 (get-internal-real-time)))
		 (with-open-file (str "out3.2b.txt" :direction :input)
	   (format t "~% Solution is correct? ~a~&" (string= (format nil "~{~a~^~}" (states-to-list (a* *p2*))) (read str))))
      (let ((real2 (get-internal-real-time)))
	  (format t "~%Computation took: ~f seconds of real time~%" (/ (- real2 real1) internal-time-units-per-second))))