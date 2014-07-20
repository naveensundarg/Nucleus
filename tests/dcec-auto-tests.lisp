(in-package :dcec-fol)

(defparameter *dcec-test-1* 
  '(@ (C now 
        (implies 
         (P jack now raining)
         (K jack now raining)))))


(defparameter *dcec-test-2* 
  '(@ (C now 
        (implies 
         (K jack now raining)
         (B jack now raining)))))
 
(defparameter *dcec-test-3*
  '(implies (@ (C t ))))
(defparameter *dcec-tests* 
  (let ((total-tests 2))
    (mapcar (lambda (n)
	      (eval 
	       (read-from-string 
		(concatenate 'string 
			     "*dcec-test-"
			     (princ-to-string n)
			     "*"))))
	    (range 1 total-tests))))


 
(defun Iequal (x y)
  (cond ((and (prop? x) (prop? y )) 
         (equalp (p-value x) (p-value y)))
        (t (equalp x y))))

(defun range (a b) (loop for i from a to b collect i))

(defun run-dcec-tests (&optional (str nil))
  (let ((count 0)
	(passed 0)
	(ignores-list nil))
    (mapcar (lambda (test-case) 
	     ; (format str "~a" test-case)
	      (if (not (member (1+ count) ignores-list))
		  (let*
		      ((I-out (funcall #'prove-DCEC* test-case))
		       (result 
                        (equalp I-out :PROOF-FOUND)))
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
	    *dcec-tests*)
     (format t 
                   "~% Dcec Tests: Total Passed ~a out of ~a." 
                   passed (- count (length ignores-list)))
     (format t "~% Ignored ~a" (length ignores-list))
     
    (force-output t)))
