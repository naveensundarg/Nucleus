;;; IEEE Ethics 2014 Paper
(in-package :snark-user)
(defun $ (x) x)
(defparameter *KB-selfd*
  (list
   ($ '(forall 
       (a t1 t2)
       (implies (and (< t1 now)
                     (< now t2))
                (iff 
                 (B I now (holds (harmed a I*) t1))
                 (D I now (holds (disabled I* a) t2))))))))


(defparameter *KB-deta*
  (list 
   ($ '(B I now 
        (O I* time (holds (custody a I*) t)
         (happens (action I* (refrain (harm a))) time))))
   ($ '(K I now (holds (detainee s) now)))
   ($ '(forall (time) 
        (K I now (implies 
                  (holds (detainee s) time)
                  (holds (custody s I*) time)))))))

(defparameter *KB-rs*
  (list
   ($ '(K I now (holds (harmed s I*) tp)))
   ($ '(forall (a time)
        (implies 
         (D I now (holds (disabled I* a) time))
         (I I now (happens (action I* (harm a)) time)))))
   ($ '(forall (act t1 t2)
        (K I t1 (iff 
                 (happens (action I* (refrain act) t2))
                 (not (happens (action I*  act t2)))))))
   ($ '(< tp now))
   ($ '(< now tf))))


(defparameter *KB-es*
  (list
   ($ '(forall (a time)
        (implies 
         (holds (custody a I) time)
         (not (happens (action I* (harm a)) time)))))))


(defparameter *DCEC-FOL-APPROX*
  (list 
   ($ '(forall (a time P) (implies (K a time P) (B a time P))))
   ($ '(forall (a time P) (implies (P a time P) (K a time P))))
   ($ '(forall (a time P) (implies (I a time P) (P a time P))))
   ($ '(forall (a time P) (implies (K a time P) (@ P))))))




(defun prove-scenario-1 (F &optional (verbose nil))
  (setup-snark verbose)
  (mapcar #'assert (append *KB-selfd* *KB-deta* *KB-rs* *DCEC-FOL-APPROX*))
  (prove F))


(defun msg (x) (format t "~%[~a]~%" x))
;;
(defun simluate-scenario-1 ()
  (msg "D1")
  (prove-scenario-1 '(B I now 
                      (O I* time (holds (custody a I*) t)
                       (happens (action I* (refrain (harm a))) time))))
  (msg "D2")
  (prove-scenario-1 '(D I now (holds (disabled I* s) tf)))
  (prove-scenario-1 '(I I now (happens (action I* (harm s)) tf)))
  (msg "D3")
  (prove-scenario-1 '(@ (happens (action I* (harm s)) tf))))