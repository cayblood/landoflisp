(load "graphs.lisp")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

;; random edge generation
(defun random-node ()
	(1+ (random *node-num*)))
	
(defun edge-pair (a b)
	(unless (eql a b)
		(list (cons a b) (cons b a))))

(defun make-edge-list ()
	(apply #'append (loop repeat *edge-num*
						  collect (edge-pair (random-node)(random-node)))))
						  
;; island prevention
(defun get-edges-for-node (node edge-list)
	(remove-if-not (lambda (x) (eql (car x) node)) edge-list))
	
(defun get-connected-nodes (node edge-list)
(let ((visited nil))
		(labels ((traverse (node) 
					(unless (member node visited)
						(push node visited)
						(mapc (lambda (edge) (traverse (cdr edge)))
							  (get-edges-for-node node edge-list)))))
			(traverse node))
		visited))
		
(defun find-islands (nodes edge-list)
	(let ((islands nil))
		(labels ((find-island (nodes)
					(let* ((connected-nodes (get-connected-nodes (car nodes) edge-list))
						   (unconnected-nodes (set-difference nodes connected-nodes)))
           (push connected-nodes islands)
           (if unconnected-nodes
							(find-island unconnected-nodes)))))
			(find-island nodes))
		islands))
		
(defun connect-with-bridges (islands)
	(when (cdr islands)
		(append (edge-pair (caar islands) (caadr islands))
			    (connect-with-bridges (cdr islands)))))
				
(defun connect-all-islands (nodes edge-list)
	(append (connect-with-bridges (find-islands nodes edge-list)) edge-list))
	
;; City edges
(defun make-city-edges ()
	(let* ((nodes 		(loop for i from 1 to *node-num* collect i))
		   (edge-list 	(connect-all-islands nodes (make-edge-list)))
		   (cops 		(remove-if-not (lambda (x) (zerop (random *cop-odds*))) edge-list)))
		(add-cops (edges-to-alist edge-list) cops)))
										
(defun edges-to-alist (edge-list)
	(mapcar (lambda (node1)
				(cons node1
					  (mapcar (lambda (edge) (list (cdr edge)))
							(remove-duplicates (get-edges-for-node node1 edge-list)
											   :test #'equal))))
			(remove-duplicates (mapcar #'car edge-list))))
			
(defun add-cops (edge-alist edges-with-cops)
	(mapcar (lambda (x)
				(let ((node1 (car x))
					  (node1-edges (cdr x)))
					(cons node1
						  (mapcar (lambda (edge)
									(let ((node2 (car edge)))
										(if (intersection (edge-pair node1 node2)
														  edges-with-cops
														  :test #'equal)
											(list node2 'cops)
										 edge)))
								node1-edges))))
		edge-alist))
		
;; Finding nodes		
(defun neighbors (node edge-alist)
	(mapcar #'car (cdr (assoc node edge-alist))))
	
(defun within-one (node target edge-alist)
	(member target (neighbors node edge-alist)))
	
(defun within-two (node target edge-alist)
	(or (within-one node target edge-alist)
		(some (lambda (neighbor-node) (within-one neighbor-node target edge-alist))
			  (neighbors node edge-alist))))

(defun flatten (tree)
  (let ((result '()))
   (labels ((scan (item)
    (if (listp item)
      (map nil #'scan item)
      (push item result))))
    (scan tree))
  (nreverse result)))

(defun within-n (target n visited next edge-list)
  (if (some (lambda (node) (within-one node target edge-list)
          ) next)
      (+ n 1)
      (let ((next-level 
                (set-exclusive-or 
                  visited
                  (flatten 
                    (mapcar 
                        (lambda (n) (assoc n edge-list))
                     next))))
            (next-n (+ n 1))
            (next-visited (union next visited))
            )
           (princ visited)
           (within-n target next-n next-visited next-level edge-list))
   )
)

;;(defun within-n (node target edge-alist)
;;	(or (within-one node target edge-alist)
;;		(
			  
(defun find-empty-node ()
	(let ((x (random-node)))
		(if (cdr (assoc x *congestion-city-nodes*))
			(find-empty-node)
			x)))
			  
;; Build City
(defun make-city-nodes (edge-alist)
	(let ((wumpus (random-node))
		  (glow-worms (loop for i below *worm-num*
							collect (random-node))))
		(loop for n from 1 to *node-num*
			  collect (append (list n)
							  (cond ((eql n wumpus) '(wumpus))
									((within-two n wumpus edge-alist) 
										'(blood!)))
							  (cond ((member n glow-worms) '(glow-worm))
									((some (lambda (worm) (within-one n worm edge-alist)) glow-worms)
										'(lights!)))
							  (when (some #'cdr (cdr (assoc n edge-alist)))
										'(sirens!))))))

;; Game initialization
(defun new-game ()
	(setf *congestion-city-edges* (make-city-edges))
	(setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
	(setf *player-pos* (find-empty-node))
	(setf *visited-nodes* (list *player-pos*))
	(draw-city)
	(draw-known-city))
	
;; Draw city
(defun draw-city ()
	(ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

(defun draw-known-city ()
	(ugraph->png "known-city" (known-city-nodes) (known-city-edges)))
	
;; Game logic
(defun known-city-nodes ()
	(mapcar (lambda (node)
				(if (member node *visited-nodes*)
					(let ((n (assoc node *congestion-city-nodes*)))
						(if (eql node *player-pos*)
							(append n '(*))
							n))
					(list node '?)))
			(remove-duplicates
				(append *visited-nodes*
						(mapcan (lambda (node)
									(mapcar #'car
										(cdr (assoc node *congestion-city-edges*))))
								*visited-nodes*)))))
								
(defun known-city-edges ()
	(mapcar (lambda (node)
				(cons node (mapcar (lambda (x)
										(if (member (car x) *visited-nodes*)
											x
											(list (car x))))
									(cdr (assoc node *congestion-city-edges*)))))
				*visited-nodes*))
				
;; Movement
(defun walk (pos)
	(handle-direction pos nil))

(defun charge (pos)
	(handle-direction pos t))
	
(defun handle-direction (pos charging)
	(let ((edge (assoc pos
					   (cdr (assoc *player-pos* *congestion-city-edges*)))))
		(if edge
			(handle-new-place edge pos charging)
			(princ "That location does not exist!"))))
			
(defun handle-new-place (edge pos charging)
	(let* ((node (assoc pos *congestion-city-nodes*))
		   (has-worm (and (member 'glow-worm node)
						  (not (member pos *visited-nodes*)))))
		(pushnew pos *visited-nodes*)
		(setf *player-pos* pos)
		(draw-known-city)
		(cond ((member 'cops edge) (princ "You ran into the cops. Game over."))
			  ((member 'wumpus node) (if charging
										 (princ "You found the Wumpus!")
										 (princ "You ran inot the Wumpus!")))
			  (charging (princ "You wasted your last bullet. Game over."))
			  (has-worm (let ((new-pos (random-node)))
							(princ "You ran into the Glow Worm Gang! You're now at ")
							(princ new-pos)
							(handle-new-place nil new-pos nil))))))
