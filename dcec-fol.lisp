(in-package :snark-user)

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

(defun setup-snark (&optional  (verbose nil))
  (snark:initialize :verbose  verbose)
  (if (not verbose) (snark-deverbose))
  ;;(snark:run-time-limit  5)
  (snark:assert-supported t)
  (snark:assume-supported t)
  (snark:prove-supported t)
  (snark:use-resolution t)
  (snark:use-paramodulation t)
  (snark:allow-skolem-symbols-in-answers nil))



(defparameter *NOT*
  '(forall (P1)
    (iff (@ (not P1)) (not (@ P1)))))

(defparameter *IMPL*
  '(forall (P1 P2) 
        (iff 
         (@ (implies P1 P2)) 
         (implies (@ P1) 
          (@ P2)))))

(defparameter *AND*
  '(forall (P1 P2) 
        (iff 
         (@ (and P1 P2)) 
         (and (@ P1) 
          (@ P2)))))

(defparameter *OR*
  '(forall (P1 P2) 
        (iff 
         (@ (or P1 P2)) 
         (or (@ P1) 
          (@ P2)))))

(defparameter *R1*
  '(forall (a t F) (@ (C t 
                       (implies (P a t F)
                                (K a t F))))))

(defparameter *R2*
  '(forall (a t F) (@ (C t 
                       (implies (K a t F)
                                (B a t F))))))

(defparameter *R3*
  '(forall (t F a1 a2 a3 t1 t2 t3)
    (implies 
     (and (<p t t1) (<p t t2) (<p t t3) (@ (C t F))) 
     (@ (K a1 t1 (K a2 t2 (K a3 t3 F)))))))

(defparameter *R4*
  '(forall (a t F)
    (implies 
     (@ (K a t F)) 
     (@ F))))

(defparameter *DR1*
  '(forall (t F a1 a2 t1 t2)
    (implies 
     (and (<p t t1) (<p t t2)  (@ (C t F))) 
     (@ (K a1 t1 (K a2 t2 F))))))

(defparameter *DR2*
  '(forall (t F a1 t1)
    (implies 
     (and (<p t t1)  (@ (C t F))) 
     (@ (K a1 t1 F)))))

(defparameter *DR3*
  '(forall (t F)
    (implies 
     (@ (C t F)) 
     (@ F))))

(defparameter *DR4*
  '(forall (t a F )
    (implies 
     (@ (P a t F))
     (@ (K a t F)))))

(defparameter *DR4*
  '(forall (t a F )
    (implies 
     (@ (K a t F))
     (@ (B a t F)))))


(defparameter *time-1*
  '(forall (t) (<p t t)))


(defparameter *DCEC-FOL-PURE*
  (list *time-1*
        *NOT* *OR* *AND* *IMPL*
       ;(forall (F) '(iff (@ F) F))
        *R1* *R2* *R3* *R4*
        *DR1* *DR2* *DR3* *DR4*))

(defun prove-from-axioms (axioms f)
  (setup-snark)
  (mapcar #'assert axioms)
  (prove f))

(defun prove-DCEC* (q)
  (prove-from-axioms *DCEC-FOL-PURE* q))


(prove-DCEC* '(@ (C now 
                  (implies 
                   (P jack now raining)
                   (K jack now raining)))))