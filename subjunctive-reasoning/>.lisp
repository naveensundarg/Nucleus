(in-package :subjunctive)

(defparameter *infinity* SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY)
(defparameter *empty-wl-enumeration* '( (implies P P)))


(defun prove> (statements P>Q &key (wl-enumeration *empty-wl-enumeration* ) (time *infinity*))
  (prove>-int statements P>Q wl-enumeration time))

(defun antecedent (P>Q) (second P>Q))
(defun consequent (P>Q) (third P>Q))

(defun prove>-int (statements P>Q wl-enumeration time)
  (every (complement #'null)
         (mapcar
          (lambda (curr-wl) 
            (let ((curr-prems (append (g statements time)
                                      (list curr-wl)
                                      (list (antecedent P>Q)))))
              (if (not curr-wl)
                  t
                  (if (consistent? curr-prems time)
                      (if (prove-from-axioms 
                           curr-prems
                           (consequent P>Q))
                          t nil)
                      t))))
          wl-enumeration)))


(defparameter *ec-1*
  '(forall (f t) (implies (and 
                           (initially f)
                           (not (clipped 0 f t)))
                  (holds f t))))


(defparameter *ec-2*
  '(forall (e f t1 t2) 
     (implies
      (and (happens e t1) (initiates e f t1) (< t1 t2) (not (clipped t1 f t2)))
      (holds f t2))))

(defparameter *ec-3*
  '(forall (t1 f t2)
    '(iff (clipped t1 f t2)
      (exists (e t) (and
                     (happens e t)
                     (< t1 t)
                     (< t t2)
                     (terminates e f t))))))

(defparameter *g-test-1*
  (list 
   '(forall (a t) (initiates 
                   (action I (kick a)) 
                   (damaged a)
                   t))
   '(forall (t1 a t2) (not (clipped t1 (damaged a) t2)))
   '(not (happens (action I (kick S)) tp))
   '(forall (a t) (implies (holds (damaged a) t)
                   (exists tt (and (< tt t) (happens (action I (harmed a) tt))))))
   '(< tp now)))
