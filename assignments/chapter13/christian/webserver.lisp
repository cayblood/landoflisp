;; http escape codes
(defun http-char (char1 char2 &optional (default #\space))
	(let ((code (parse-http-code char1 char2)))
		(if code
			(code-char code)
			default)))

(defun parse-http-code (char1 char2)
	(parse-integer (coerce (list char1 char2) 'string) :radix 16 :junk-allowed t))
		
			
(defun decode-param (input-string)
	(labels ((f (lst)
				(when lst
					(case (car lst)
						(#\% (cons (http-char (cadr lst) (caddr lst)) (f (cdddr lst))))
						(#\+ (cons #\space (f (cdr lst))))
						(otherwise (cons (car lst) (f (cdr lst))))))))
		(coerce (f (coerce input-string 'list)) 'string)))
		
;; handle input parameters
(defun parse-params (input-string)
	(let* ((i1 (position #\= input-string))
		   (i2 (position #\& input-string)))
		(cond 	(i1 (cons (cons (intern (string-upcase (subseq input-string 0 i1)))
								(decode-param (subseq input-string (1+ i1) i2)))
						  (and i2 (parse-params (subseq input-string (1+ i2))))))
				((equal s "") nil)
				(t input-string))))
				
;; request header parsing
(defun parse-url (input-string)
	(let* ((url (subseq input-string
						(+ 2 (position #\space input-string))
						(position #\space input-string :from-end t)))
		   (x (position #\? url)))
		(if x
			(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
			(cons url '()))))
			
(defun get-header (stream)
	(let* ((line (read-line stream))
		   (keyvalue (get-keyvalue-from-line line)))
		(when keyvalue
			(cons keyvalue (get-header stream)))))
		   
(defun get-keyvalue-from-line (line)
	(let ((index (position #\: line)))
		(when index
			(cons (intern (string-upcase (subseq line 0 index)))
				  (subseq line (+ index 2))))))
				  
(defun get-content-params (stream header)
	(let ((length (cdr (assoc 'content-length header))))
		(when length
			(let ((content (make-string (parse-integer length))))
				(read-sequence content stream)
				(parse-params content)))))
				
;; start point - main server
(defun serve (request-handler)
	(let ((socket (socket-server 8080)))
		(unwind-protect
			(loop (with-open-stream (stream (socket-accept socket))
						(let* ((url (parse-url (read-line stream)))
							   (path (car url))
							   (header (get-header stream))
							   (params (append (cdr url)
											   (get-content-params stream header)))
							   (*standard-output* stream))
									(funcall request-handler path header params))))
			(socket-server-close socket))))
			
;; sample web page
(defun hello-request-handler (path header params)
	(if (equal path "greeting")
		(let ((name (assoc 'name params)))
			(if (not name)
				(princ "<html><form>What is your name?<input name='name' /></form></html>")
				(format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
		(princ "Sorry... I don't know that page.")))
		