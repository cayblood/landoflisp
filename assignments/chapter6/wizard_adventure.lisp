(defparameter *nodes* '((living-room (you are in the living room.
                            a wizard is snoring loudlly on the couch.))
                        (garden (you are in the a beatiful garden.
                            there is a well in front of you.))
                        (attic (you ari in the attic.
                            there is a giant welding touch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garder (living-room east door)
                        (attic (living-room downstairs ladder)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defparameter *location* 'living-room)

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun describe-location (location nodes)
    (cadr (assoc location nodes)))

(defun describe-path (edge)
    `(there is a, (caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
    (labels ((at-loc-p (obj)
            (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
    (labels ((describe-object (obj)
            `(you see ,obj an the floor.)))
        (apply #'append (mapcar #'describe-object (objects-at loc objs obj-loc)))))

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
    (cond ((member object 
                    (objects-at *location* *objects* *object-locations*))
          (push (list object 'body) *object-locations*)
            `(you are now carrying the ,object))
          (t '(you cannot get that.))))

(defun inventory () 
    (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun game-read ()
    (let ((cmd (read-from-string 
                    (concatenate 'string "(" (read-line) ")"))))
               (flet ((quote-it (x)
                                (list 'quote x)))
                  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))


(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
    (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
    (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
          ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
          ((eq item #\")(tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
          ((or caps lit)(cons (char-upcase item) (tweak-text rest nil lit)))
          (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() "
                                                    (prin1-to-string lst))
                                       'list)
                               t
                               nil)
                   'string))
    (fresh-line))

(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

