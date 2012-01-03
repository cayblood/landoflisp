;; Game mechanics

(defparameter *nodes* 
	'((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
	  (garden (you are in a beautiful garden. there is a well in front of you.))
	  (attic (you are in the attic. there is a giant welding torch in the corner.))))

(defparameter *edges*
	'((living-room (garden west door)
				   (attic upstairs ladder))
	  (garden (living-room east door))
	  (attic (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
								   (bucket living-room)
								   (chain garden)
								   (frog garden)))
								   
(defparameter *location* 'living-room)
	  
(defun describe-location (location nodes)
	(cadr (assoc location nodes)))
	
(defun describe-path (edge)
	`(there is a ,(caddr edge) going ,(cadr edge) from here.))
	
(defun describe-paths (location edges)
	(apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))
	
(defun objects-at (location objects object-locations)
	(labels ((at-location-p (object)
				(eq (cadr (assoc object object-locations)) location)))
		(remove-if-not #'at-location-p objects)))
		
(defun describe-objects (location objects object-locations)
	(labels ((describe-object (object)
				`(you see a ,object on the floor.)))
		(apply #'append (mapcar #'describe-object (objects-at location objects object-locations)))))

(defun look ()
	(append (describe-location *location* *nodes*)
			(describe-paths *location* *edges*)
			(describe-objects *location* *objects* *object-locations*)))
			
(defun walk (direction)
	(let ((next (find direction
					(cdr (assoc *location* *edges*))
					:key #'cadr)))
	(if next
		(progn (setf *location* (car next))
			   (look))
		'(you cannot go that way.))))
		
(defun pickup (object)
	(cond (
		(member object (objects-at *location* *objects* *object-locations*))
			(push (list object 'body) *object-locations*) 
			`(you are now carrying the ,object))
		(t '(you cannot get that.))))
		   
(defun inventory ()
	(cons 'items- (objects-at 'body *objects* *object-locations*)))

;; Game repl
(defparameter *allowed-commands* '(look walk pickup inventory))
	
(defun game-repl ()
	(let ((command (game-read)))
		(unless (eq (car command) 'quit)
			(game-print (game-eval command))
			(game-repl))))

(defun game-read ()
	(let ((command (read-from-string (concatenate 'string "(" (read-line) ")"))))
		(flet ((quote-it (x) (list 'quote x)))
			(cons (car command) (mapcar #'quote-it (cdr command))))))
			
(defun game-eval (command)
	(if (member (car command) *allowed-commands*)
		(eval command)
		'(i do not know that command)))
		
(defun tweak-text (sentence is-capital is-literal)
	(when sentence
		(let ((item (car sentence))
			  (rest (cdr sentence)))
			(cond ((eq item #\space) 			(cons item (tweak-text rest is-capital is-literal)))
				  ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t is-literal)))
				  ((eq item #\") 				(tweak-text rest is-capital (not is-literal)))
				  (is-literal 					(cons item (tweak-text rest nil is-literal)))			  
				  (is-capital					(cons (char-upcase item) (tweak-text rest nil is-literal)))  ;; Removed redundant or
				  (t (cons (char-downcase item) (tweak-text rest nil nil)))))))
			  
(defun game-print (output)
	(princ (coerce (tweak-text (coerce (string-trim "() " 
													(prin1-to-string output))
										'list) 
								t 
								nil) 
					'string))
	(fresh-line))

;; chapter 17 DSL

(defun have (object)
	(member object (inventory)))
	
(defparameter *chain-welded* nil)
(defparameter *bucket-filled* nil)

(defmacro game-action (command subj obj place &body body)
	(let ((subject (gensym))
		  (object (gensym)))
		`(progn (defun ,command (,subject ,object)
					(if (and (eq *location* ',place)
							 (eq ,subject ',subj)
							 (eq ,object ',obj)
							 (have ',subj))
						,@body
					'(i cant ,command like that.)))
				(pushnew ',command *allowed-commands*))))
			
(game-action weld chain bucket attic
			 (if (and (have 'bucket) (not *chain-welded*))
				(progn (setf *chain-welded* t)
					   '(the chain is now securely welded to the bucket.))
				'(you do not have a bucket.)))

(game-action dunk bucket well garden
			 (if *chain-welded*
				(progn (setf *bucket-filled* t)
					   '(the bucket is now full of water))
				'(the water level is too low to reach.)))