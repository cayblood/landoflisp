(setf *print-circle* t)

(defparameter foo '(1 2 3))
(setf (cdddr foo) foo))