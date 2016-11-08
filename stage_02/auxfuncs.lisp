; This is not an official file, use it at your own risk. 
; This is not an official file, use it at your own risk. 
; This is not an official file, use it at your own risk. 
; This is not an official file, use it at your own risk. 
; This is not an official file, use it at your own risk. 
; This is not an official file, use it at your own risk. 
; This is not an official file, use it at your own risk. 
; This is not an official file, use it at your own risk. 
; This is not an official file, use it at your own risk. 
; This is not an official file, use it at your own risk. 
; This is not an official file, use it at your own risk. 


(defun orderlistofcoordinates (coordlist)
  ;; order list of coordinates
  (stable-sort (copy-alist coordlist)
	       #'(lambda (x y)
		   (> (+ (* (car x) 10)
			 (second x))
		      (+ (* (car y) 10)
			 (second y))))))

(defun loadtrack (trackname)
  ;; load a trackX.txt file into a Track structure
  (format t "Loading track: ~a" trackname)

  (setq track (make-Track) )

  (let ((in (open trackname :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	  while line do 
	    (let ( (lineaux ()) )
	      (loop for c across line do
		    (if (eq c #\Return) (return))
		    (setf lineaux
		      (nconc 
		       lineaux
		       (list c))))
	      ;;process outoftrack states
	      (nsubstitute nil #\X lineaux)
	      (nsubstitute t #\0 lineaux)
	      (loop 
		(if (not (position #\E lineaux)) (return))
		(setf (Track-endpositions track)
		  (nconc (Track-endpositions track)
			 (list (list (length (Track-env track)) (position #\E lineaux)))))
		(setf (nth (position #\E lineaux) lineaux) t))
	      (loop
		(if (not (position #\S lineaux)) (return))
		(setf (Track-startpos track)
		  (nconc (Track-startpos track)
			 (list (list (length (Track-env track)) (position #\S lineaux)))))
		(setf (nth (position #\S lineaux) lineaux) t))
	      (setf (Track-env track)
		(nconc (Track-env track) (list lineaux)))
	      (setf (Track-size track) (list (length (Track-env track)) (length (car (Track-env track)))))))
      (close in)))

  (setf (Track-endpositions track)
    (orderlistofcoordinates (Track-endpositions track)))
  
  (setf (Track-startpos track) (first (Track-startpos track)))

  (return-from loadtrack track))
