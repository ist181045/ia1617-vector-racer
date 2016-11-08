
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
(load "datastructures.lisp")
(load "auxfuncs.lisp")

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
;  ((if (not (null lst))
;    (if (equal coord (car lst))
;      t
;      (coordInList coord (cdr lst))
;    )
;    nil
;  ))
;)

(defun nextState (st act)
  (let* (
      (v_t
        (mapcar #'+ (state-vel st) act))
      (p_t
        (mapcar #'+ (state-pos st) v_t))
      (state
        (make-state
          :pos p_t
          :vel v_t
          :action act
          :cost 1
          :track (state-track st)
          :other (state-other st)))
    )
    (cond (
      (isObstaclep p_t (state-track state))
        (setf
          (state-pos state) (state-pos st)
          (state-vel state) '(0 0)
          (state-cost state) 20
        )
      )(
      (isGoalp state)
        (setf
          (state-cost state) -100
        )
      )
    )
    state
  )
)
;  (make-STATE :POS '(3 16)
;        :VEL '(1 3)
;        :ACTION act
;        :COST -100))
