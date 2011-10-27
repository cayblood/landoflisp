(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;; Board creation
(defun board-array (lst)
	(make-array *board-hexnum* :initial-contents lst))
	
(defun build-board ()
	(board-array (loop for n below *board-hexnum*
					   collect (list (random *num-players*)(1+ (random *max-dice*))))))
					   
(defun get-ascii-from-player-number (n)
	(code-char (+ 97 n)))
	
(defun draw-board (board)
	(loop for y below *board-size*
		do (progn (fresh-line)
				  (loop repeat (- *board-size* y)
						do (princ "  "))
				  (loop for x below *board-size*
						for hex = (aref board (+ x (* *board-size* y)))
						do (format t "~a-~a " (get-ascii-from-player-number (first hex))
											  (second hex))))))
;; Rules handling
(defun game-tree (board player spare-dice first-move)
	(list player
		  board
		  (add-passing-move board
							player
							spare-dice
							first-move
							(attacking-moves board player spare-dice))))
							
