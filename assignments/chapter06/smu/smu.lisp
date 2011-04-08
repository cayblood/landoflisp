(mapcar (lambda (n) (* n n)) 
        '(1 2 3 4 5 6))

(defun start ()
  (print "Enter a number")
  (let ((n(read)))
    (mapcar (lambda (num) (print (* n num))) '(1 2 3))
  )
)

(start)