



(defparameter *propositional-test-1*
  (list  
   (list '(both (right-and ($ (and P Q))) (left-and ($ (and P Q)))) 
         (list ($ '(and P Q))))
   ($ '(and Q P))))


(defparameter *propositional-test-2*
  (list  
   (list '(both (left-and ($ (and Q P))) (right-and ($ (and Q P)))) 
         (list ($ '(and Q P))))
   ($ '(and Q P))))


(defparameter *propositional-test-3*
  (list 
   (list '(assume ($ (and P Q)) in 
           (both 
            (right-and ($ (and P Q))) 
            (left-and ($ (and P Q))))) 
         nil)
   ($'(implies (and P Q) (and Q P)))))

(defparameter *propositional-test-4* 
  (list 
   (list '(commutative-and ($ (and P Q))) 
         (list ($ '(and P Q))))
   ($ '(and Q P))))

(defparameter *propositional-test-5*
  (list 
   (list '(assume ($ P) in 
           (assume ($ (implies P Q)) in 
            (modus-ponens ($ P) ($ (implies P Q)))))
         nil)
   ($ '(implies P (implies (implies P Q) Q)))))

(defparameter *propositional-test-6*
  (list 
   (list '(assume ($ (and Q P)) in 
           (commutative-and ($ (and Q P))))
         nil)
   ($ '(implies (and Q P) (and P Q)))))

(defparameter *propositional-test-7*
  (list 
   (list '(assume ($ (not (not P))) in (double-negation ($ (not (not P)))))
         nil)
   ($ '(implies (not (not P)) P))))

(defparameter *propositional-test-8*
  (list 
   (list '(constructive-dilemma ($ (or P1 P2)) ($ (implies P1 Q)) ($ (implies P2 Q)))
         (list ($ '(or P1 P2)) ($ '(implies P1 Q)) ($'(implies P2 Q))))
   ($ 'Q)))

(defparameter *propositional-test-9*
  (list (list '(equivalence ($ (implies P Q)) ($ (implies Q P)))
              (list ($ '(implies P Q)) ($ '(implies Q P))))
        ($ '(iff P Q))))

(defparameter *propositional-test-10*
  (list (list '(assume ($ (iff P Q)) in (left-iff ($ (iff P Q))))
              nil)
        ($ '(implies (iff P Q) P))))

(defparameter *propositional-test-11*
  (list (list '(assume ($ (iff P Q)) in (right-iff ($ (iff P Q))))
              nil)
        ($ '(implies (iff P Q) Q))))

(defparameter *propositional-test-12*
  (list (list '(absurd ($ P) ($ (not P)))
              (list ($ 'P) ($ '(not P))))
        ($ 'false)))


(defparameter *propositional-test-13*
  (list 
   (list '(assume ($ P) in
           (assume ($ Q) in
            (claim ($ P))))
         nil)
   ($ '(implies P (implies Q P))))
  "P=>(Q=>P)")


(defparameter *propositional-test-14*
  (list 
   (list 
    `(assume 
      ($ H) in
      (suppose-absurd 
       ($ Ma) in
       (dseq 
        (modus-ponens ($ H) ($ (implies H (and E D))))
        (left-and ($ (and E D)))
        (left-either ($ E) ($ My))
        (modus-ponens ($ (or E My)) ($ (implies (or E My) R)))
        (modus-ponens ($ Ma) ($ (implies Ma (not R))))
        (absurd ($ R) ($ (not R))))))
    (list ($ '(implies H (and E D)))
          ($ '(implies (or E My) R))
          ($ '(implies Ma (not R)))))
   ($ '(implies H (not Ma))))
  "kok_o213_8_32")

(defparameter *propositional-test-15*
  (list (list 
         '(dlet ((P1 ($ (implies H (and E D))))
                 (P2 ($ (implies (or E My) R)))
                 (P3 ($ (implies Ma (not R)))))
                in
           (assume ($ H) in
                   (suppose-absurd ($ Ma) in
                                   (dseq 
                                    (modus-ponens ($ H) P1)
                                    (left-and ($ (and E D)))
                                    (left-either ($ E) ($ My))
                                    (modus-ponens ($ (or E My)) P2)
                                    (modus-ponens ($ Ma) P3)
                                    (absurd ($ R) ($ (not R)))))))
         (list ($ '(implies H (and E D)))
               ($ '(implies (or E My) R))
               ($ '(implies Ma (not R)))))
        ($ '(implies H (not Ma))))
  "same as 14 but with dlet")





(defparameter *propositional-test-16*
  (list 
   (list 
    `(dlet ((P1 ($ (implies H (and E D))))
            (P2 ($ (implies (or E My) R)))
            (P3 ($ (implies Ma (not R))))) 
           in
           (assume ($ H) in
                   (dlet ((D1 (modus-ponens ($ H) P1)))
                         in
                         (suppose-absurd ($ Ma) in
                                         (dseq 
                                          (left-and ($ (and E D)))
                                          (left-either ($ E) ($ My))
                                          (modus-ponens ($ (or E My)) P2)
                                          (modus-ponens ($ Ma) P3)
                                          (absurd ($ R) ($ (not R))))))))
    (list ($ '(implies H (and E D)))
          ($ '(implies (or E My) R))
          ($ '(implies Ma (not R)))))
   ($ '(implies H (not Ma))))
  "nested dlet and using a deduction's result in the body of the dlet")



(defparameter *propositional-test-17* 
  (list 
   (list
    '(dlet 
      ((Premise1 ($ (and (implies H (and E Ma))
                         (implies (not H) (and (not E) (not Ma))))))
       (Premise2 ($ (implies (not H) (not My)))))
      in 
      (assume Premise1 in
       (assume Premise2 in
        (dseq 
         (assume ($ H) in 
                 (dseq  
                  (left-and Premise1)
                  (modus-ponens ($ H) ($ (implies H (and E Ma))))
                  (right-and ($ (and E Ma)))
                  (left-either ($ Ma) ($ MY))))
         (assume 
          ($ (or Ma My)) in  
          (dseq 
           (right-and Premise1)
           (assume 
            ($ Ma) in
            (dseq 
             (suppose-absurd 
              ($ (not H)) in	
              (dseq 
               (right-and Premise1)
               (modus-ponens ($ (not H)) ($ (implies (not H) (and (not E) (not Ma)))))
               (right-and ($ (and (not E) (not Ma))))
               (absurd ($ Ma) ($ (not Ma)))))
             (double-negation ($ (not (not H))))))
           (assume 
            ($ My) in
            (dseq 
             (suppose-absurd
              ($ (not H)) in
              (dseq 
               (modus-ponens ($ (not H)) Premise2)
               (absurd ($ My) ($ (not My)))))
             (double-negation ($ (not (not H))))))
           (constructive-dilemma ($ (or Ma My)) ($ (implies Ma H)) ($ (implies My H)))))
         (equivalence ($ (implies H (or Ma My))) ($ (implies (or Ma My) H)))))))
    nil)
   ($ '(implies (and 
                 (implies H (and E Ma))
                 (implies (not H) (and (not E) (not Ma))))
        (implies (implies (not H) (not My))
         (iff H (or Ma My)))))))


(defparameter *propositional-test-18*
  (list
   (list 
    '(dlet ((P1 ($ (implies (not Cube_b) Small_b)))
            (P2 ($ (implies Small_c (or Small_d Small_e))))
            (P3 ($ (implies Small_d (not Small_c))))
            (P4 ($ (implies Cube_b (not Small_e))))) in
      (assume ($ Small_c) in
       (dseq 
        (suppose-absurd 
         ($ Small_d) in
         (dseq
          (modus-ponens ($ Small_d) P3)
          (absurd ($ Small_c) ($ (not Small_c)))))
        (modus-ponens ($ Small_c) P2)
        (assume ($ Small_e) in (claim ($ Small_e)))
        (assume 
         ($ Small_d) in
         (dseq 
          (suppose-absurd 
           ($ (not Small_e)) in
           (absurd ($ Small_d) ($ (not Small_d))))
          (double-negation ($ (not (not Small_e))))))
        (suppose-absurd 
         ($ Cube_b) in
         (dseq
          (constructive-dilemma ($ (or Small_d Small_e)) ($ (implies Small_d Small_e)) ($ (implies Small_e Small_e)))
          (modus-ponens ($ Cube_b) P4)
          (absurd ($ Small_e) ($ (not Small_e)))))
        (modus-ponens ($ (not Cube_b)) P1))))
    (list ($ '(implies (not Cube_b) Small_b))
          ($ '(implies Small_c (or Small_d Small_e)))
          ($ '(implies Small_d (not Small_c)))
          ($ '(implies Cube_b (not Small_e)))))
   ($ '(implies Small_c Small_b))))



(defun range (a b) (loop for i from a to b collect i))



(defparameter *propositional-tests* 
  (let ((total-tests 18))
    (mapcar (lambda (n)
	      (eval 
	       (read-from-string 
		(concatenate 'string 
			     "*propositional-test-"
			     (princ-to-string n)
			     "*"))))
	    (range 1 total-tests))))


(defun Iequal (x y)
  (cond ((and (prop? x) (prop? y )) 
         (equalp (p-value x) (p-value y)))
        (t (equalp x y))))
(defun run-propositional-tests (&optional (str nil))
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
	    *propositional-tests*)
    (push (format nil 
                  "~% Propositional Tests: Total Passed ~a out of ~a." 
                  passed (- count (length ignores-list)))
          *reports* )
    (push (format nil "~% Ignored ~a" (length ignores-list))
          *reports* )
    (force-output t)))

 