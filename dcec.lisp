(define-primitive-method claim (P)
  (check-in-base P B))

(define-primitive-method modus-ponens (antecedent implication)
  (flet ((consequent (P) (third P)))
    (check-in-base antecedent B)
    (check-in-base implication B)
    (match (p-value implication)
      ((guard imp (and (is-conditional? imp)
                       (matches `(implies ,(p-value antecedent) _) imp))) 
       (consequent (p-value implication))))))

(define-primitive-method modus-tollens (nconsequent implication)
  (check-in-base nconsequent B)
  (check-in-base implication B)
  (match (p-value implication)
    ((and (list 'implies ant conseq) (equalp conseq (p-value nconsequent))) 
     (@prop `(not ,ant)))))

(define-primitive-method or-intro (P Q)
  (check-in-base-or B P Q)
  (@prop `(or ,P ,Q)))


(define-primitive-method left-and (P)
  (check-in-base P B) 
  (match (p-value P) ((list 'and left _) ($ left))))


(define-primitive-method right-and (P)
  (check-in-base P B)
  (match (p-value P) ((list 'and _ right) ($ right))))

(define-primitive-method and-intro (P Q)
  (check-in-base P B)
  (check-in-base Q B)
  (@prop `(and ,P ,Q)))




(define-method commutative-and (x)
  (! and-intro (! right-and x) (! left-and x)))


