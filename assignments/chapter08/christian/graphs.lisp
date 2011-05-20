(defparameter *wizard-nodes* '(	(living-room 	(you are in the living-room. a wizard is snoring loudly on the couch.))
								(garden			(you are in a beautiful garden. there is a well in front of you.))
								(attic			(you are in the attic. there is a giant welding torch in the corner.))))
								
(defparameter *wizard-edges* '( (living-room	(garden west door)
												(attic upstairs ladder))
								(garden			(living-room east door))
								(attic			(living-room downstairs ladder))))

(defparameter *max-label-length* 30)
								
(defun dot-name (expression)
	(substitute-if #\_ (complement #'alphanumericp) (prin1-to-string expression)))

(defun dot-label (expression)
	(if expression
		(let ((s (write-to-string expression :pretty nil)))
			(if (> (length s) *max-label-length*)
				(concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...") 
				s))
		""))
		
(defun nodes->dot (nodes)
	(mapc (lambda (node)
			(fresh-line)
			(princ (dot-name (car node)))
			(princ "[label=\"")
			(princ (dot-label node))
			(princ "\"];"))
		  nodes))
		  
(defun edges->dot (edges)
	(mapc (lambda (node)
		(mapc (lambda (edge)
				(fresh-line)
				(princ (dot-name (car node)))
				(princ "->")
				(princ (dot-name (car edge)))
				(princ "[label=\"")
				(princ (dot-label (cdr edge)))
				(princ "\"];"))
			  (cdr node)))
		edges))
		
(defun graph->dot (nodes edges)
	(princ "digraph{")
	(nodes->dot nodes)
	(edges->dot edges)
	(princ "}"))
	
(defun dot->png (filename thunk)
	(with-open-file 
		(*standard-output*
		 filename
		 :direction :output
		 :if-exists :supersede)
		(funcall thunk))
	(ext:shell (concatenate 'string "dot -Tpng -O " filename)))
	
(defun graph->png (filename nodes edges)
	(dot->png filename (lambda () (graph->dot nodes edges))))
	

;; Undirected graph 

(defun uedges->dot (edges)
	(maplist 	(lambda (lst)
					(mapc 	(lambda (edge)
								(unless (assoc (car edge) (cdr lst))
									(fresh-line)
									(princ (dot-name (caar lst)))
									(princ "--")
									(princ (dot-name (car edge)))
									(princ "[label=\"")
									(princ (dot-label (cddr edge)))  ;; changed to cddr since having the direction in a ugraph didn't make sense
									(princ "\"];")))
							(cdar lst)))
				edges))

(defun ugraph->dot (nodes edges)
	(princ "graph{")
	(nodes->dot nodes)
	(uedges->dot edges)
	(princ "}"))
	
(defun ugraph->png (filename nodes edges)
	(dot->png filename (lambda () (ugraph->dot nodes edges))))