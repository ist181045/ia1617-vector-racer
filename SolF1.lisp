
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
(load "scripts/datastructures.lisp")
(load "scripts/auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
;(load "datastructures.fas")
;(load "auxfuncs.fas")

(defun isObstaclep (pos track)
	(not
		(nth (cadr pos) (nth (car pos) (track-env track)))))

(defun isGoalp (st)
	;(= (state-cost st) -100)
	;(coordInList (state-pos st) (track-endpositions (state-track st)))
	(not (null
			(member
				(state-pos st)
				(track-endpositions (state-track st))
				:test #'equal))))

;(defun coordInList (coord lst)
;	((if (not (null lst))
;		(if (equal coord (car lst))
;			t
;			(coordInList coord (cdr lst))
;		)
;		nil
;	))
;)

(defun nextState (st act)
	(let* (
			(v_t
				(list
					(+ (car (state-vel st)) (car act))
					(+ (cadr (state-vel st)) (cadr act))))
			(p_t
				(list
					(+ (car (state-pos st)) (car v_t))
					(+ (cadr (state-pos st)) (cadr act))))
			(state
				(make-state
					:pos p_t
					:vel v_t
					:action act
					:cost 1
					:other (state-other st)))
		)
		(cond
			((isObstaclep p_t (state-track track))
				(setf
					(state-pos state) (state-pos st)
					(state-vel state) '(0 0)
					(state-cost state) 20)
			)
			((isGoalp state)
				(state-cost state -100)
			)
			(t state)
		)
		state
	)
)
;  (make-STATE :POS '(3 16)
;	      :VEL '(1 3)
;	      :ACTION act
;	      :COST -100))
