(in-package :subjunctive)




(defparameter *world-statement-forms*
  (list  '(happens ?e ?t)
         '(holds ?f ?t)
         '(initially ?)))

(defun g (statements time)
  "computes general laws, g(statements) from statements bounded by time"
  (g-int statements time ))

(defun g-int (statements time )
  (if (null statements)
      '()
      (if (general? statements time)
          statements
          (reduce
           (lambda (prev curr)
             (let* ((smaller
                        (g-int (set-difference statements (list curr) 
                                               :test  #'equalp)
                               time))
                       (curr-candidate (cons curr smaller)))
               (let ((curr-ans (if (general? curr-candidate time)
                                    curr-candidate
                                    smaller)))
                 (if (< (length prev) (length curr-ans))
                     curr-ans prev))))
           statements :initial-value ()))))


(defun general? (statements time)
  "Is the given set of statements general bounded by time?"
  (let ((ind-time (/ time (length *world-statement-forms*))))
    (general?-int statements time *world-statement-forms*)))


(defun general?-int (statements time world-statement-forms)
  (if (null world-statement-forms)
      t
      (if (or
           (prove-from-axioms 
            statements 
            `(snark::not ,(first world-statement-forms))
            :time-limit time)
           (prove-from-axioms 
               statements 
               (first world-statement-forms)
               :time-limit time))
           nil
           (general?-int 
            statements 
            time 
            (rest world-statement-forms)))))



