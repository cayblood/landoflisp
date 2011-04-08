;; Squaring a list of values with a lambda
(defun squareListWithLambda (numbers)
	(mapcar (lambda (n) (* n n)) numbers))

;; Squaring a list with external function
(defun squareList (numbers)
	(mapcar #'squareSingleNumber numbers))

(defun squareSingleNumber (n)
	(* n n))

;; Squaring a list with a label	
(defun squareListWithLabel (numbers)
	(labels ((squareLabels (n) (* n n)))
		(mapcar #'squareLabels numbers)))

;; Function taking a function as a parameter
(defun doSomething (func numbers)
	(mapcar func numbers))
	
;; Passing multiple functions in as a list		
(defun dostuff ()
	(doAll 
		(list #'squareSingleNumber
			  (lambda (n) (+ n n)))
		'(1 2 3)))
	
(defun doAll (funclist numbers)
	(mapcar (lambda (f) (mapcar f numbers)) funclist))

;; Passing multiple functions which have no arguments
(defun doStuffNoArgs ()
	(doAllNoArguments
		(list #'dostuff (lambda () (* 2 2)))))

(defun doAllNoArguments (funclist)
	(mapcar #'funcall funclist))
	