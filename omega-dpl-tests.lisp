



(defparameter *test-1*
  (list  
   '((! and-intro (! right-and (and P Q)) (! left-and (and P Q))) (list (and P Q)))
   '(and Q P)))


(defparameter *test-2*
  (list  
   '((! and-intro (! left-and (and Q P)) (! right-and (and Q P))) (list (and Q P)))
   '(and Q P)))


(defun range (a b) (loop for i from a to b collect i))

(defparameter *omega-dpl-tests* 
  (let ((total-tests 2))
    (mapcar (lambda (n)
	      (eval 
	       (read-from-string 
		(concatenate 'string 
			     "*test-"
			     (princ-to-string n)
			     "*"))))
	    (range 1 total-tests))))


(defun run-tests (&optional (str nil))
  (let ((count 0)
	(passed 0)
	(ignores-list nil))
    (mapcar (lambda (test-case) 
	     ; (format str "~a" test-case)
	      (if (not (member (1+ count) ignores-list))
		  (let*
		      ((I-out (apply #'I  (first test-case)))
		       (result (equalp I-out (second test-case) )))
		    (format str 
			    "--------~%Test Case ~a: ~%   ~a~%   ===>~%   ~a ~%   Passed? ~a~%" 
			    (1+ count) 
			    (first test-case)
			    I-out
			    (if result
				(progn (incf passed) "Yes.")   
				"NO" ))
		    (force-output str)
		    (incf count))
		  (progn (format str "  [   Ignoring test case ~a]~%" 
				 (1+ count))
			 (force-output str)
			 (incf count))))
	    *omega-dpl-tests*)
    (format t "~% Total Passed ~a out of ~a." passed (- count (length ignores-list)))
    (format t "~% Ignored ~a" (length ignores-list))
    (force-output t)))

(defun run-all-tests (&optional (verbose nil))
  (format t "~% --- RUNNING TESTS --- ~%" )
  (force-output t)
  (time (run-tests verbose)))

