



(defparameter *fol-test-1* 
  (list 
   (list
    '(assume ($ (forall (x) (P x))) in
      (specialize ($ (forall (x) (P x))) with a))
    nil)
   ($ '(implies (forall (x) (P x)) (P a)))))


(defparameter *fol-test-2*
  (list 
   (list 
    '(assume ($ (P a)) in
      (ex-generalize ($ (exists (x) (P x))) from a))
    nil)
   ($ '(implies (P a) (exists (x) (P x))))))


(defparameter *fol-test-3*
  (list 
   (list 
    '(assume ($ (forall (x) (implies (Man x) (Mortal x)))) in
      (assume ($ (Man Socrates)) in
       (dseq
        (specialize ($ (forall (x) (implies (Man x) (Mortal x))))
                    with Socrates)
        (! modus-ponens ($ (Man Socrates)) 
           ($ (implies (Man Socrates)
                       (Mortal Socrates)))))))    
    nil)
   ($ '(implies (forall (x) (implies (man x) (mortal x)))
        (implies (man socrates) (mortal socrates))))))

(defparameter *fol-tests* 
  (let ((total-tests 3))
    (mapcar (lambda (n)
	      (eval 
	       (read-from-string 
		(concatenate 'string 
			     "*fol-test-"
			     (princ-to-string n)
			     "*"))))
	    (range 1 total-tests))))


(defun Iequal (x y)
  (cond ((and (prop? x) (prop? y )) 
         (equalp (p-value x) (p-value y)))
        (t (equalp x y))))


(defun run-fol-tests (&optional (str nil))
  (let ((count 0)
	(passed 0)
	(ignores-list nil))
    (mapcar (lambda (test-case) 
	     ; (format str "~a" test-case)
	      (if (not (member (1+ count) ignores-list))
		  (let*
		      ((I-out (apply #'I  (first test-case)))
		       (result 
                         (Iequal I-out (second test-case) )))
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
	    *fol-tests*)
    (push (format nil 
                  "~% Fol Tests: Total Passed ~a out of ~a." 
                  passed (- count (length ignores-list)))
          *reports* )
    (push (format nil "~% Ignored ~a" (length ignores-list))
          *reports* )
    (force-output t)))
