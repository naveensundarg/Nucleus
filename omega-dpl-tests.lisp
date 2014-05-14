



(defparameter *test-1*
  (list  
   (list '(! both (! right-and ($ (and P Q))) (! left-and ($ (and P Q)))) 
         (list ($ '(and P Q))))
   ($ '(and Q P))))


(defparameter *test-2*
  (list  
   (list '(! both (! left-and ($ (and Q P))) (! right-and ($ (and Q P)))) 
         (list ($ '(and Q P))))
   ($ '(and Q P))))


(defparameter *test-3*
  (list 
   (list '(assume ($ (and P Q)) in 
           (! both 
            (! right-and ($ (and P Q))) 
            (! left-and ($ (and P Q))))) 
         nil)
   ($'(implies (and P Q) (and Q P)))))

(defparameter *test-4* 
  (list 
   (list '(! commutative-and ($ (and P Q))) 
         (list ($ '(and P Q))))
   ($ '(and Q P))))

(defparameter *test-5*
  (list 
   (list '(assume ($ P) in 
           (assume ($ (implies P Q)) in 
            (! modus-ponens ($ P) ($ (implies P Q)))))
         nil)
   ($ '(implies P (implies (implies P Q) Q)))))

(defparameter *test-6*
  (list 
   (list '(assume ($ (and Q P)) in 
           (! commutative-and ($ (and Q P))))
         nil)
   ($ '(implies (and Q P) (and P Q)))))

(defparameter *test-7*
  (list 
   (list '(assume ($ (not (not P))) in (! double-negation ($ (not (not P)))))
         nil)
   ($ '(implies (not (not P)) P))))

(defparameter *test-8*
  (list 
   (list '(! constructive-dilemma ($ (or P1 P2)) ($ (implies P1 Q)) ($ (implies P2 Q)))
         (list ($ '(or P1 P2)) ($ '(implies P1 Q)) ($'(implies P2 Q))))
   ($ 'Q)))

(defparameter *test-9*
  (list (list '(! equivalence ($ (implies P Q)) ($ (implies Q P)))
              (list ($ '(implies P Q)) ($ '(implies Q P))))
        ($ '(iff P Q))))

(defparameter *test-10*
  (list (list '(assume ($ (iff P Q)) in (! left-iff ($ (iff P Q))))
              nil)
        ($ '(implies (iff P Q) P))))

(defparameter *test-11*
  (list (list '(assume ($ (iff P Q)) in (! right-iff ($ (iff P Q))))
              nil)
        ($ '(implies (iff P Q) Q))))

(defparameter *test-12*
  (list (list '(! absurd ($ P) ($ (not P)))
              (list ($ 'P) ($ '(not P))))
        ($ 'false)))


(defparameter *test-13*
  (list 
   (list '(assume ($ P) in
           (assume ($ Q) in
            (! claim ($ P))))
         nil)
   ($ '(implies P (implies Q P))))
  "P=>(Q=>P)")


(defparameter *test-14*
  (list 
   (list 
    `(assume 
      ($ H) in
      (suppose-absurd 
       ($ Ma) in
       (dseq 
        (! modus-ponens ($ H) ($ (implies H (and E D))))
        (! left-and ($ (and E D)))
        (! left-either ($ E) ($ My))
        (! modus-ponens ($ (or E My)) ($ (implies (or E My) R)))
        (! modus-ponens ($ Ma) ($ (implies Ma (not R))))
        (! absurd ($ R) ($ (not R))))))
    (list ($ '(implies H (and E D)))
          ($ '(implies (or E My) R))
          ($ '(implies Ma (not R)))))
   ($ '(implies H (not Ma))))
  "kok_o213_8_32")


(defun range (a b) (loop for i from a to b collect i))



(defparameter *omega-dpl-tests* 
  (let ((total-tests 14))
    (mapcar (lambda (n)
	      (eval 
	       (read-from-string 
		(concatenate 'string 
			     "*test-"
			     (princ-to-string n)
			     "*"))))
	    (range 1 total-tests))))


(defun Iequal (x y)
  (cond ((and (prop? x) (prop? y )) 
         (equalp (p-value x) (p-value y)))
        (t (equalp x y))))
(defun run-tests (&optional (str nil))
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
	    *omega-dpl-tests*)
    (format t "~% Total Passed ~a out of ~a." passed (- count (length ignores-list)))
    (format t "~% Ignored ~a" (length ignores-list))
    (force-output t)))

(defun run-all-tests (&optional (verbose nil))
  (format t "~% --- RUNNING TESTS --- ~%" )
  (force-output t)
  (time (run-tests verbose)))
