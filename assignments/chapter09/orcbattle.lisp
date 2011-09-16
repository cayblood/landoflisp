(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

;; GAME START
(defun orc-battle ()
	(init-monsters)
	(init-player)
	(game-loop)
	(when (player-dead)
		(princ "You have been killed. Game over!"))
	(when (monsters-dead)
		(princ "Congratulations! You have vanquished all of your foes")))

(defun init-player ()
	(setf *player-health* 30)
	(setf *player-agility* 30)
	(setf *player-strength* 30))
	
(defun init-monsters ()
	(setf *monsters*
		(map 'vector
			(lambda (x) (funcall (nth (random (length *monster-builders*)) *monster-builders*)))
			(make-array *monster-num*))))
	
;; GAME LOOP		
(defun game-loop ()
	(unless (or (player-dead) (monsters-dead))
		(show-player)
		;; loop is run i times, i is assigned by the get-num-of-player-attacks value
		(dotimes (i (get-num-of-player-attacks))
			(unless (monsters-dead)
				(show-monsters)
				(player-attack)))
		(fresh-line)
		(monsters-attack)
		(game-loop)))

;; attacks = 1 + (the greater of 0 or agility) divided by 15, rounded down	
(defun get-num-of-player-attacks ()
	(1+ (truncate (/ (max 0 *player-agility*) 15))))
	
(defun monsters-attack()
	(map 'list
		(lambda(m) (or (monster-dead m) (monster-attack m)))
		*monsters*))

;; GAME STATE
(defun player-dead ()
	(<= *player-health* 0)
	
(defun monster-dead (m)
	(<= (monster-health m) 0))
	
(defun monsters-dead ()
	(every #'monster-dead *monsters*))
	
(defun show-player ()
	(fresh-line)
	(princ "You are a valiant knight with a health of ")
	(princ *player-health*)
	(princ ", an agility of ")
	(princ *player-agility*)
	(princ ", and a strength of ")
	(princ *player-strength*))

(defun show-monsters ()
	(fresh-line)
	(princ "Your foes:")
	(let ((x 0))
		(map 'list
			(lambda (m)
				(fresh-line)
				(princ "   ")
				(princ (incf x))
				(princ ". ")
				(if (monster-dead m)
					(princ "**dead**")
					(progn 
						(princ "(Health=")
						(princ (monster-health m))
						(princ ") ")
						(monster-show m))))
			*monsters*)))
	
;; GAME ACTIONS
(defun player-attack ()
	(fresh-line)
	(princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
	(case (read)
		(s (stab))
		(d (double-swing))
		(otherwise (roundhouse))))

;; Stab							
(defun stab ()
	(monster-hit (pick-monster) (get-stab-attack-strength)))
							
(defun get-stab-attack-strength ()
	(+ 2 (randval (ash *player-strength* -1))))

;; Double Swing	
(defun double-swing ()
	(let ((x (get-double-swing-attack-strength)))
		(princ "Your double swing has a strength of ")
		(princ x)
		(fresh-line)
		(monster-hit (pick-monster) x)
		(unless (monsters-dead)
			(monster-hit (pick-monster) x))))

(defun get-double-swing-attack-strength ()
	(randval (truncate (/ *player-strength* 6 ))))
		
;; Roundhouse
(defun roundhouse ()	
	(dotimes (x (get-roundhouse-attack-strength))
		(unless (monsters-dead)
			(monster-hit (random-monster) 1))))		
		
(defun get-roundhouse-attack-strength ()
	(1+ (randval (truncate (/ *player-strength* 3)))))
		
;; MONSTER SELECTION
(defun random-monster ()
	(let ((m (aref *monsters* (random (length *monsters*)))))
		(if (monster-dead m)
			(random-monster)
			m)))
			
(defun pick-monster ()
	(fresh-line)
	(princ "Monster #:")
	(let ((x (read)))
		(if (is-available-monster-index x)
			(let ((m (aref *monsters* (1- x))))
				(if (monster-dead m)
					(progn (princ "That monster is already dead.")
						(pick-monster))
					m))
			(progn (princ "That is not a valid monster number.")
				(pick-monster)))))

(defun is-available-monster-index (x)
	(and (integerp x) (>= x 1) (<= x *monster-num*)))

;; MONSTERS
(defstruct monster (health (randval 10)))

(defmethod monster-hit (m dmg)
	(decf (monster-health m) dmg)
	(if (monster-dead m)
		(progn
			(princ "You killed the ")
			(princ (type-of m))
			(princ "! "))
		(progn
			(princ "You hit the ")
			(princ (type-of m))
			(princ ", knocking off ")
			(princ dmg)
			(princ " health points! "))))
		
		