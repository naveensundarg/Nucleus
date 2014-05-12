(ql:quickload "cl-unification")
(in-package :snark-user)
(import 'cl-unification:match)

(defparameter *B* ())

(defun check (base proof)
  (let* ((*B* base))
    (check-int  proof)))

(defun check-int ( proof)
  (if proof
      (let ((curr (atomic-proof-check (first proof))))
	(let ((*B* (cons curr *B*)))
	  (check-int (rest proof))))
      t))

(defun atomic-proof-check (step)
  (let ((rule (first step)) 
	(goal (second step))
	(inputs  (rest step)))
    (if  (apply rule inputs)
	goal (error "~a failed on ~a ~a" rule goal inputs))))



(defun in-base (f) (if (member f *B* :test #'equalp
			       ) t (error "~a not in base ~a" f *B*)))


(defmacro $ (&rest args) `',@args)

($ a b c)

(defun R1 (conc a time F) 
  (equalp conc `(C ,time (implies (P ,a ,time ,F) (K ,a ,time ,F)))))


(defun R2 (conc a time F) 
  (equalp conc `(C ,time (implies (K ,a ,time ,F) (B ,a ,time ,F)))))

(defun R3-1 (conc a C) 
  (match ('(?K (C ?time ?F)) (list conc C))
      (equalp conc `(K ,a ,?time ,?F))))

(defun R4 (conc K)
  (in-base K)
  (match ('(?F (K ?a ?time ?F)) (list conc K))
    ?F))

(defun R11* (conc B1 B2)
  (match ('((B ?a ?time ?F1) (B ?a ?time ?F2) (B ?a ?time ?F))
	   (list B1 B2 conc))
    (declare (ignore a))
    (if (prove-with (list F1 F2) F)
	(list 'B ?a ?time ?F) 
	(error "R11* failed on ~a ~a ~a" conc B1 B2))))

(defun K-to-B.d (conc K)
  (match  '(((B ?a ?time ?F) (K ?a ?time ?F))
	    conc K) ) (R2 ) )

(defun fol (goal &rest premises)
  (prove-with premises goal))










(defun prove-with (premises goal)
  (setup-snark)
  (let ((premises (append *B* premises)))
    (mapcar #'snark::assert premises)
       (if (equalp :PROOF-FOUND (prove goal))
	   goal 
	   nil;(error "prove-with failed on ~a ~a" premises goal)
	   )))

(defun snark-deverbose ()
  (snark:print-options-when-starting  nil)
  (snark:print-agenda-when-finished nil)
  (snark:print-clocks-when-finished nil)
  (snark:print-final-rows nil)
  (snark:print-symbol-table-warnings nil)
  (snark:print-summary-when-finished nil)
  (snark:print-row-answers nil)
  (snark:print-row-goals nil) 
  (snark:print-rows-when-derived nil)
  (snark:print-row-reasons nil)
  (snark:print-row-partitions nil)
  (snark:print-rows-prettily nil)
  (snark:print-rows :min 0 :max 0))

(defun setup-snark (&key  (verbose nil) (paramodulation nil) )
  (snark:initialize :verbose  verbose)
  (if (not verbose) (snark-deverbose))
  
  (snark:assert-supported t)
  (snark:assume-supported t)
  (snark:prove-supported t)
  (snark:use-resolution t)
;  (snark:use-paramodulation paramodulation)
;  (use-subsumption-by-false t)
  )




(defun simple-unify (x y)
  (handler-case
      (if  (cl-unification:unify x y) t)
    (cl-unification:UNIFICATION-FAILURE  () nil)))