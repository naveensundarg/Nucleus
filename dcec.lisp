(defmacro pmatch (prop &rest body) 
  `(let () (match (p-value ,prop) ,@body
                  (_ (error "could not match ~a"  (p-value ,prop) )))))

(defmacro have? (P) `(check-in-base ,P B))
(define-primitive-method claim (P)
  (have? P))


(defmacro {} () '(declare (ignore B)))
(define-primitive-method modus-ponens (antecedent implication)
  (flet ((consequent (P) (third P)))
    (have? antecedent)
    (have? implication)
    (pmatch implication
      ((guard imp (and (is-conditional? imp)
                       (matches `(implies ,(p-value antecedent) ,(consequent imp)) imp))) 
       (@prop (consequent imp))))))

(define-primitive-method modus-tollens (nconsequent implication)
  (have? nconsequent)
  (have? implication)
  (match (p-value implication)
    ((and (list 'implies ant conseq) (equalp conseq (p-value nconsequent))) 
     (@prop `(not ,ant)))))

(define-primitive-method double-negation (P)
  (have? P)
  (pmatch P ((list 'not (list 'not Q)) Q)))

(define-primitive-method both (P Q)
  (have? P)
  (have? Q)
  (@prop `(and ,P ,Q)))

(define-primitive-method left-and (P)
  (have? P) 
  (match (p-value P) ((list 'and left _) ($ left))))

(define-primitive-method right-and (P)
  (check-in-base P B)
  (match (p-value P) ((list 'and _ right) ($ right))))


(define-primitive-method left-either (P Q)
  (check-in-base P B)
  (@prop `(or ,P ,Q)))

(define-primitive-method right-either (P Q)
  (check-in-base Q B)
  (@prop `(or ,P ,Q)))

(define-primitive-method constructive-dilemma (disjunct left right)
  (check-in-base disjunct B)
  (check-in-base left B)
  (check-in-base right B)
  (let ((bindings 
         (unify '((or ?P1 ?P2) (implies ?P1 ?Q) (implies ?P2 ?Q))
                (list (p-value disjunct)  (p-value left) (p-value right) ))))
    (@prop (@ '?Q bindings))))


(define-primitive-method equivalence (left right)
  (have? left)
  (have? right)
  (let ((bindings 
         (unify '((implies ?P1 ?P2) (implies ?P2 ?P1))
                (list (p-value left) (p-value right)))))
    (@prop `(iff ,(@ '?P1 bindings) ,(@ '?P2 bindings)))))


(define-primitive-method left-iff (iffP)
  (have? iffP)
  (let ((bindings (unify '(iff ?P1 ?P2) (p-value iffP))))
    (@prop (@ '?P1 bindings))))

(define-primitive-method right-iff (iffP)
  (have? iffP)
  (let ((bindings (unify '(iff ?P1 ?P2) (p-value iffP))))
    (@prop (@ '?P2 bindings))))

(define-primitive-method absurd (P Q)
  (have? P) (have? Q)
  (unify '(?P1 (not ?P1))
         (list (p-value P) (p-value Q)))
  ($ 'false))

(define-primitive-method true-intro () (declare (ignore B)) ($ 'true))
(define-primitive-method false-elim () (declare (ignore B))($ '(not false)))


(define-primitive-method =-intro (a)
  (declare (ignore B))
  (@prop `(= ,a ,a)))

(define-primitive-method =-intro (a)
  (declare (ignore B))
  (@prop `(= ,a ,a)))


;; Other convenient primitive rules


;; Derived methods

(define-method commutative-and (x)
  (! both (! right-and x) (! left-and x)))
;;; Modal methods



(define-primitive-method R1 (a time P)
  (declare (ignore B))
  ($ `(C ,time (implies (P ,a ,time ,P) (K ,a ,time ,P)))))

(define-primitive-method R2 (a time P)
    (declare (ignore B))
    (@prop`(C ,time (implies (P ,a ,time ,P) (B ,a ,time ,P)))))

(define-primitive-method R3 (C agents times)
  (check-in-base C B)
  (pmatch C 
          ((list 'C time P)
           (if (mapcar (lambda (tt) (check-in-base (@prop `(< ,time ,tt)) B)) times)
            ($ (reduce (lambda (prev agent-time)
                         `(K ,(first agent-time) ,(second agent-time) ,prev))
                       (reverse (zip agents times))
                       :initial-value P))))
          (otherwise (error "Not common knowledge: ~a" C))))

(define-primitive-method R4 (Knows)
  (check-in-base Knows B)
  (pmatch Knows
          ((list 'k _ _ P)
           ($ P))
          (otherwise "Not knowledge: ~a" knows)))


(define-primitive-method R5 (time a t1 t2 t3 P1 P2)
  (declare (ignore B))
  ($ `(C ,time
         (implies (K ,a ,t1 (implies ,P1 ,P2))
                  (implies (K ,a ,t2 ,P1)
                           (K ,a ,t3 ,P2))))))

(define-primitive-method R6 (time a t1 t2 t3 P1 P2)
  (declare (ignore B))
  ($ `(C ,time
         (implies (B ,a ,t1 (implies ,P1 ,P2))
                  (implies (B ,a ,t2 ,P1)
                           (B ,a ,t3 ,P2))))))


(define-primitive-method R7 (time t1 t2 t3 P1 P2)
  (declare (ignore B))
  ($ `(C ,time
         (implies (C ,t1 (implies ,P1 ,P2))
                  (implies (C ,t2 ,P1)
                           (C ,t3 ,P2))))))

(define-primitive-method R8 (P time term)
  (declare (ignore B))
  (pmatch P
          ((list 'forall _ _)
           ($ `(C ,time (implies ,(p-value P)
                                ,(subst-var 
                                 (top-var (p-value P)) term 
                                 (kernel (p-value P)))))))
          (otherwise (error "Not a universal:~a"P))))

(define-primitive-method R9 (time P1 P2)
 (declare (ignore B))
 (@prop `(C ,time (implies (iff ,P1 ,P2)
                           (implies (not ,P2) (not ,P1))))))


;;; R10 missing
;;; R11 missing
;;; R12 missing


