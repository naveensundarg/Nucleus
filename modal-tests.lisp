
(defparameter *modal-test-1*
  (list 
   (list '(R1 'a 't 'P) nil)
   ($ '(C t (implies (P a t p) (K a t p))))))

(defparameter *modal-test-2*
  (list 
   (list '(R2 'a 't 'P) nil)
   ($ '(C t (implies (P a t p) (B a t p))))))


(defparameter *modal-test-3*
  (list 
   (list '(assume ($ (C time P)) in (R3 ($ (C time P)) '(a1 a2 a3) '(t1 t2 t3)))
         (list ($ '(< time t1))
               ($ '(< time t2))
               ($ '(< time t3))))
   ($ '(implies (C time p) (K a1 t1 (K a2 t2 (K a3 t3 p)))))))



(defparameter *modal-test-4*
  (list 
   (list '(assume ($ (K a time P)) in
           (R4 ($ (K a time P))))
         nil)
   ($ '(implies (K a time P) P))))


(defparameter *modal-test-5*
  (list 
   (list '(R5 't 'a 't1 't2 't3 'P1 'P2 )
         nil)
   ($ '(C t (implies (K a t1 (implies p1 p2)) 
             (implies (K a t2 p1) (K a t3 p2)))))))

(defparameter *modal-test-6*
  (list 
   (list '(R6 't 'a 't1 't2 't3 'P1 'P2 )
         nil)
   ($ '(C t (implies (B a t1 (implies p1 p2)) 
             (implies (B a t2 p1) (B a t3 p2)))))))

(defparameter *modal-test-7*
  (list 
   (list '(R7 't 'a 't1 't2 't3 'P1 'P2 )
         nil)
   ($ '(C t (implies (C t1 (implies p1 p2)) 
             (implies (C t2 p1) (C t3  p2)))))))


(defparameter *modal-tests* 
  (let ((total-tests 7))
    (mapcar (lambda (n)
	      (eval 
	       (read-from-string 
		(concatenate 'string 
			     "*modal-test-"
			     (princ-to-string n)
			     "*"))))
	    (range 1 total-tests))))




(defun run-modal-tests (&optional (str nil))
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
	    *modal-tests*)
    (push (format nil 
                  "~% Modal Tests: Total Passed ~a out of ~a." 
                  passed (- count (length ignores-list)))
          *reports* )
    (push (format nil "~% Ignored ~a" (length ignores-list))
          *reports* )
    (force-output t)))
