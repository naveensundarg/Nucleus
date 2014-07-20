(in-package :dcec-fol)


(defun setup-snark (&optional  (verbose nil))
  (snark:initialize :verbose  verbose)
  (if (not verbose) (snark-deverbose))
  ;;(snark:run-time-limit  5)
  (snark:assert-supported t)
  (snark:assume-supported t)
  (snark:prove-supported t)
  (snark:use-hyperresolution t)
  (snark:use-paramodulation t)
  (snark:allow-skolem-symbols-in-answers nil))


(defun declare-sorts ()
  (declare-sort 'Formula)
  (declare-subsort 'Knowledge 'Formula)
  (declare-subsort 'Belief 'Formula)
  (declare-sort 'Agent )
  (declare-subsort 'Moment 'Number)
  (declare-relation '@ 1 :sort '(Formula)))

(defun declare-logic-functors ()
  (declare-function 'implies! 2 :sort '(Formula Formula Formula))
  (declare-function 'iff! 2 :sort '(Formula Formula Formula))
  (declare-function 'and! 2 :sort '(Formula Formula Formula))
  (declare-function 'or! 2 :sort '(Formula Formula Formula))
  (declare-function 'not! 1 :sort '(Formula Formula)))

(defun declare-modal-functors ()
  (declare-function 'C 2 :sort '(Formula Moment Formula))
  (declare-function 'P 3 :sort '(Formula Agent Moment Formula))
  (declare-function 'K 3 :sort '(Formula Agent Moment Formula))
  (declare-function 'B 3 :sort '(Formula Agent Moment Formula)))


(defun declare-all-sorts-and-functors ()
  (declare-sorts)
  (declare-logic-functors)
  (declare-modal-functors))

(defparameter *NOT*
  '(forall ((?P Formula))
    (iff (@ (not! ?P)) (not (@ ?P)))))

(defparameter *IMPL*
  '(forall ((?P1 Formula) (?P2 Formula)) 
        (iff 
         (@ (implies! ?P1 ?P2)) 
         (implies (@ ?P1) 
          (@ ?P2)))))

(defparameter *AND*
  '(forall ((?P1 Formula) (?P2 Formula)) 
        (iff 
         (@ (and! ?P1 ?P2)) 
         (and (@ ?P1) 
          (@ ?P2)))))

(defparameter *OR*
  '(forall ((?P1 Formula) (?P2 Formula)) 
        (iff 
         (@ (or! ?P1 ?P2)) 
         (or (@ ?P1) 
          (@ ?P2)))))

(defparameter *R1*
  '(forall ((?a Agent) (?t Moment) (?F Formula)) 
    (@ (C ?t 
        (implies! (P ?a ?t ?F)
                 (K ?a ?t ?F))))))

(defparameter *R2*
  '(forall ((?a Agent) (?t Moment) (?F Formula))
    (@ (C ?t 
        (implies! (K ?a ?t ?F)
                 (B ?a ?t ?F))))))

(defparameter *R3-2*
  '(forall ((?t Moment)
            (?F Formula) 
            (?a1 Agent) (?a2 Agent)
            (?t1 Moment) (?t2 Moment))
    (implies 
     (and (< ?t ?t1) (< ?t ?t2)   (@ (C ?t ?F))) 
     (@ (K ?a1 ?t1 (K ?a2 ?t2 ?F))))))

(defparameter *R4*
  '(forall ((?a Agent) (?t Moment) (?F Formula))
    (implies 
     (@ (K ?a ?t ?F)) 
     (@ ?F))))

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
        *R1* *R2* *R3-2* *R4*
      ;  *DR1* *DR2* *DR3* *DR4*
        ))


(defun prove-from-axioms (axioms f &key constants verbose)
  (setup-snark verbose)
  (declare-all-sorts-and-functors)
  (mapcar (lambda (decl) (apply #'declare-constant decl)) constants)
  (mapcar #'snark::assert axioms)
  (snark:prove f))

(defun prove-DCEC* (premises q &key constants verbose)
  (prove-from-axioms (append premises *DCEC-FOL-PURE*) q :constants constants
  :verbose verbose))



