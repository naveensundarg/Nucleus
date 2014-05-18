



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


(defparameter *fol-test-4*
  (list 
   (list '(pick-any a in 
	       (dseq 
		(specialize ($ (forall (x) (and (P x) (Q x)))) with a)
		(! right-and ($ (and (P a) (Q a))))
		(! left-and ($ (and (P a) (Q a))))
		(! both ($ (Q a)) ($ (P a)))))
         (list ($ '(forall (x) (and (P x) (Q x))))))
   ($ '(forall (?z7) (and (q ?z7) (p ?z7))))))


(defparameter *fol-test-5*
  (list 
   (list '(assume ($ (forall (x) (P x))) in
	      (suppose-absurd ($ (exists (x) (not (P x)))) in
	       (pick-witness y for ($ (exists (x) (not (P x)))) in
		(dseq
		 (specialize ($ (forall (x) (P x))) with y)
		 (! absurd ($ (P y))($ (not (P y))))))))
         nil)
   ($ '(implies (forall (x) (P x)) (not (exists (x) (not (P x))))))))


(defparameter *fol-test-6*
  (list 
   (list '(pick-any y in
           (dseq 
            (specialize ($ (forall (x) (and (P x) (Q x)))) with y)
            (! right-and ($ (and (P y) (Q y))))
            (specialize ($ (forall (x) (implies (Q x) (R x)))) with y)
            (! modus-ponens ($ (Q y)) ($ (implies (Q y) (R y))))))
         (list ($ '(forall (x) (and (P x) (Q x))))
	       ($ '(forall (x) (implies (Q x) (R x))))))
   ($ '(forall (?y) (R ?y)))))

(defparameter *fol-test-7*
  (list 
   (list 
    '(assume ($ (forall (x) (implies (P y) (Q x)))) in
      (assume ($ (P y)) in
       (pick-any z in
        (dseq 
         (specialize ($ (forall (x) (implies (P y) (Q x)))) with z)
         (! modus-ponens ($ (p y)) ($ (implies (p y) (q z)))))))))
   ($ '(implies (forall (x) (implies (P y) (Q x)))
               (implies (P y) (forall (z) (Q z)))))))

(defparameter *fol-test-8*
  (list 
   (list '(pick-witness z for ($ (exists (x) (and (R x) (P x)))) in
	      (dseq
	       (! right-and ($ (and (R z) (P z))))
	       (specialize ($ (forall (x) (implies (P x) (Q x)))) with z)
	       (! modus-ponens ($ (P z)) ($ (implies (P z) (Q z))))
	       (! left-and ($ (and (R z) (P z))))
	       (! both ($ (Q z)) ($ (R z)))
	       (ex-generalize ($ (exists (y) (and (Q y) (R y)))) from z)))
	    (list ($ '(forall (x) (implies (P x) (Q x))))
		  ($ '(exists (x) (and (R x) (P x))))))
   ($ '(exists (y) (and (Q y) (R y))))))

(defparameter *fol-tests* 
  (let ((total-tests 8))
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
                        (F= (p-value I-out) (p-value (second test-case)) )))
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
