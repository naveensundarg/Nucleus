(in-package :dcec)

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
   (list '(R7 't  't1 't2 't3 'P1 'P2 )
         nil)
   ($ '(C t (implies (C t1 (implies p1 p2)) 
             (implies (C t2 p1) (C t3  p2)))))))


(defparameter *modal-test-8*
  (list 
   (list '(R8 ($ (forall (x) (Q x))) 't 'a) nil)
   ($ '(C T (IMPLIES (FORALL (X) (Q X)) (Q A))))))

(defparameter *modal-test-9*
  (list 
   (list '(R9 'time ($ P1) ($ P2)) nil)
   ($ '(C time (implies (iff P1 P2) (implies (not P2) (not P1)))))))

(defparameter *modal-test-10*
  (list
   (list '(assume ($ (implies P Q)) in
           (assume ($ (B a t P)) in
            (R11a ($ (B a t P)) ($ (implies P Q)))))
         nil)
   ($ '(implies (implies P Q) (implies (B a t P) (B a t Q))))))

(defparameter *modal-test-11*
  (list 
   (list '(assume ($ (B a t P1)) in
           (assume ($ (B a t P2)) in
            (R11b ($ (B a t P1)) ($ (B a t P2)))))
         nil)
   ($ '(implies (B a t P1) 
        (implies (B a t P2) 
         (B a t (and P1 P2)))))))

(defparameter *modal-test-12*
  (list 
   (list '(assume ($ (S p q t raining)) in
           (R12 ($ (S p q t raining))))
         nil)
   ($ '(implies (S p q t raining) (B q t (B p t raining))))))



(defparameter *modal-test-13*
  (list 
   (list '(assume ($ (I jack t (happens (action (* jack) eat) t1))) in
           (R13 ($ (I jack t (happens (action (* jack) eat) t1)))))
         nil)
   ($ '(implies (I Jack t (happens (action (* Jack) eat) t1))
        (P Jack time1 (happens (action (* Jack) eat) t1))))))


(defparameter *modal-tests* 
  (let ((total-tests 13))
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
                        (F= (p-value I-out) (p-value (second test-case)))))
		    (format str 
			    "--------~%Test Case ~a: Passed? ~a~%" 
			    (incf *count*) 
			  ;  (first test-case)
			  ;  I-out
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
