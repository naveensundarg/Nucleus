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


