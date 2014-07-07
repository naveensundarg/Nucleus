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

