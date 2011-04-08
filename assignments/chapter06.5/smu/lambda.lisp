
(defun start ()
  (print "Enter a number")
  (let ((n(read)))
    (mapcar 
      (lambda (num) (print (* n num)))
      '(1 2 3))))

(start)
