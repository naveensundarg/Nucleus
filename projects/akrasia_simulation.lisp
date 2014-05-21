;;; IEEE Ethics 2014 Paper

(defparameter *KB-selfd*
  (list
   ($ '(forall 
       (a t1 t2)
       (implies (and (< t1 now)
                     (< now t2))
                (iff 
                 (B I now (holds (harmed a I*) t1))
                 (D I now (holds (harmed a I*) t2))))))))


(defparameter *KB-deta*
  (list 
   ($ '(B I now (forall (a time)
                 (O I* time (holds (custody a I*) t)
                  (happens (action I* (refrain (harm a))) time)))))
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
         (D I now (holds (disable I* a) time))
         (I I now (happens (action I* (harm a)) time)))))
   ($ '(forall (act t1 t2)
        (K I t1 (iff 
                 (happens (action I* (refrain act) t2))
                 (not (happens (action I*  act t2)))))))))


(defparameter *KB-es*
  (list
   ($ '(forall (a time)
        (implies 
         (holds (custody a I) time)
         (not (happens (action I* (harm a)) time)))))))




(defparameter *scenario-1*
  (list 
   (assume ($ (forall (a time)
                      (O I* time (holds (custody a I*) t)
                         (happens (action I* (refrain (harm a))) time)))))
   (append *KB-selfd* *KB-deta* *KB-rs*)))