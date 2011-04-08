(defun iter (fns nums)
  (mapcar 
    (lambda 
      (f) 
      (mapcar f nums)) 
    fns))

;; apply a set of functions to a list
(defun test ()
  (iter 
    (list 
      (lambda (n) (+ n n)) 
      (lambda (n) (* n n))) 
    '(1 2 3)))

(defun xyz () 
  (+ 1 1))

;; call a list of functions
(defun test2 ()
  (mapcar 
    (lambda (f) (funcall f))
    (list
      #'xyz
      (lambda () (+ 2 2)))))

