(in-package :omega-dpl)

(defparameter *B* ())
(defparameter *primitive-methods* (make-hash-table))
(defparameter *derived-methods* (make-hash-table))


(defclass proposition () ((value :initarg :value  :accessor p-value)))
(defmethod print-object ((proposition proposition) stream)
  (format stream "[~a]"(p-value proposition)))


(defun is-proposition? (proposition)
  (flet ((syntax-check (P)  
           (or  t (symbolp P) 
               (optima:match P
                 ((or (list 'or _ _) (list 'and _ _) (list 'not _ )
                      (list 'implies _ _)
                      (list 'iff _ _)
                      (list 'forall _ _)
                      (list 'exists _ _))
                  t)
                 (_ nil)))))
     (syntax-check proposition)))
(defun $ (p) (if (is-proposition? p) 
                 (make-instance 'proposition :value p)
                 (error "~a not well formed." p)))

(defun dpl-error (msg) (error msg))

(defun is-deduction? (F)
  (or (if (consp F)  (is-method? (first F)))
      (optima:match F
        ((cons (or '! 'pick-any 'ex-generalize 'specialize 'pick-witness) _) t)
        (_ nil))))

(defun body-is-malformed? (body)
  (optima:match body 
    ((cons '! _))))

(defmacro define-primitive-method (name args &rest body)
  `(setf (gethash ',name *primitive-methods*) 
	 (lambda ,(append (list 'B) args ) ,@body)))

(defmacro define-method (name args ded)
  `(if (is-deduction? ',ded)
       (progn (setf (gethash ',name *derived-methods*)
		    (list ',args ',ded))
	      ',name)
      (error "Malformed deduction:~a" ',ded)))

(defun eval-fun-args (phrases B I)
  (mapcar (lambda (F) (funcall I F B)) phrases))

(defun eval-meth-args (phrases B I)
  (let ((ded-results nil))
    (list  
     (mapcar (lambda (F) 
	       (let ((value (apply I (list F B))))
		 (if (is-deduction? F) 
		     (push value ded-results))
		 value))
	     phrases)
     ded-results)))

(defun check-in-base (P B)  
  (if (not (member P (remove nil B) :test
                   #'(lambda (x y) (F= (p-value x)  (p-value y)))))
      (error "~a not in the assumption base." P)
      P))

(defun check-in-base-or (B &rest props)  
  (if (some  (complement #'null) 
             (mapcar 
              (lambda (P) (member P B :test #'(lambda (x y) (equalp (p-value x)  (p-value y)))))
              props))
      t
      (error "None of ~a in the assumption base." props)))

(check-in-base-or (list ($ 'P)) ($ 'P) ($'Q))


(defun is-primitive-method? (E) 
  (gethash E *primitive-methods*))

(defun is-derived-method? (E) 
  (or (gethash E *derived-methods*)
      (optima:match E 
	((list 'phi _ _) t)
	(_ nil))))

(defun is-method? (E)
  (or (is-primitive-method? E)
      (is-derived-method? E)))
(defun is-function? (E)
  (and (fboundp E) (not (macro-function E))))


(defun is-conditional? (P) (optima:match P
                             ((list 'implies _ _) P)
                             (_ nil))) 
(defun matches (pat obj) (equalp obj pat))

(defun @prop (x)
  (labels ((head (p) (first p))
         (tail (p) (rest p))
         (val (p) 
           (if (equalp 'proposition (type-of p))
               (p-value p)
               (if (atom p) p
                   (cons (val (head p)) (mapcar #'val (rest p)))))))
    (cond
      ((equal 'proposition (type-of x)) x)
      ((atom x) ($ x))
      (t ($ (cons (val (head x)) 
                  (mapcar (lambda (y) (val y))
                          (tail x))))))))

(defun quantifier (quantifiedF)
  (optima:match quantifiedF 
    ((list 'forall _ _) 'forall)
    ((list 'exists _ _) 'exists)))

(defun conn (F)
  (optima:match F 
    ((list (or 'and 'or 'iff 'implies) _ _) (first F))
    ((list 'not _) 'not)))

(defun kernel (quantifiedF)
  (optima:match quantifiedF 
    ((list (or 'forall 'exists) _ K) K)))

(defun top-var (quantifiedF)
  (optima:match quantifiedF 
    ((list (or 'forall 'exists) vars _) (first vars))))

(defun rest-vars (quantifiedF)
  (optima:match quantifiedF 
    ((list 'forall vars _) (rest vars))
    ((list 'exists vars _) (rest vars))))

(defun mapply (m values) (apply (gethash m *primitive-methods*) values))

(defun dapply (m B values) 
  (let ((def (if (symbolp m)
		 (gethash m *derived-methods*)
		 (optima:match m ((list 'phi args ded) (list args ded))))))
    (I (subst* (zip (first def) values) (second def)) B)))

(defun prop? (x)  (equalp 'proposition (type-of x)))
(defun false? (x) (sym= (p-value x) 'false))
(defparameter *trace* nil)

(defun dseq (B Deds)
  (let ((Bseq B)) 
    (reduce 
     (lambda (x y) 
       (setf Bseq (cons x Bseq))
       (I y Bseq)) Deds :initial-value ($ 'true))))
(defun dlet-select-deds (answers bindings)
  (remove nil (mapcar (lambda (ans binding)
                        (optima:match (second binding)
                          ((optima:guard x (is-deduction? x)) (second ans))
                          (_ nil)))
                      answers bindings)))
(defparameter *var-counter* 0)
(defun new-var () 
  (intern (concatenate 'string "?Z" (princ-to-string (incf *var-counter*)))))

(defun specialize (Univ B term)
  (let* ((univ-evaled (I Univ B))
         (syn (p-value univ-evaled)))
    (if (check-in-base univ-evaled B)
        (if (rest-vars syn)
            (@prop (list (quantifier syn ) 
                         (rest-vars syn) 
                         (subst term  (top-var syn) (kernel syn))))
            (@prop (subst term  (top-var syn) (kernel syn)))))))

(defun ex-generalize (Exists B term)
  (let* ((exists-evaled (I Exists B))
         (syn (p-value exists-evaled)))
    (if (check-in-base (@prop (subst term (top-var syn) (kernel syn))) B)
        exists-evaled)))

(defun pick-any (x D B)
  (let ((uvar (new-var)))
    (@prop `(forall (,uvar) ,(I (subst-var x uvar D) B)))))

(defun einstantiate (F e)
  (optima:match (p-value F)
    ((list 'exists vars K)
     (let ((subbed (subst-var (first vars) e K)))
       ($  (if (rest vars)
               `(exists ,(rest vars) ,subbed)
               subbed))))))
(defun pick-witness (x Exists D B)
  (check-in-base Exists B)
  (let* ((evar (new-var))
         (inst (einstantiate  Exists evar)))
    (I (subst-var x evar D) (cons inst B))))

(defun handle-method (E args B)
  (destructuring-bind (Values Bp)
      (eval-meth-args args B #'I)
    (cond ((is-primitive-method? E)
           (mapply E (cons (append B Bp) Values )))
          ((is-derived-method? E)
           (dapply E (append B Bp) Values))
          (t (error "~a is not a method." E)))))
(defun I (F &optional (B *B*))
  (if *trace* (format t "~a ~a" F B))
  (let ((*B* B))
    (optima:match F
      ;Clause 1: (! m args)
      ((cons '! (cons E  args)) (handle-method E args *B*))
      ((optima:guard x (if (consp x) (is-method? (first x))))
       (handle-method (first F) (rest F) *B*))
      ;; Clause 2: (assume E in D)
      ((list 'assume E 'in D)
       (let* ((P (I E B))
	      (Q (I D (cons P B))))
	 (@prop `(implies ,P ,Q))))
      ;; Clause 3: (suppose-absurd E in D)
      ((list 'suppose-absurd E 'in D)
       (let*((abs (I E B))
             (P (I D (cons abs B))))
         (if (false? P)
             (@prop `(not ,abs))
             (error "suppose-absurd failed, got ~a" P))))
      ;; Quantifiers
      ((list 'specialize Univ 'with term) 
       (specialize Univ B term))
      ((list 'ex-generalize Exists 'from term) 
       (ex-generalize Exists B term))
      ((list 'pick-any x 'in D)
       (pick-any x D B))
      ((list 'pick-witness x 'for Exists 'in D)
       (pick-witness x (I Exists B) D B))
      ;; Clause 4: (dlet ((I1 D1) (I2 D2)) in D)
      ((list 'dlet bindings 'in D)
       (let ((evaluated-bindings 
              (mapcar (lambda (binding)
                        (list (first binding) 
                              (I (second binding) B)))
                      bindings)))
         (I (subst* evaluated-bindings D) 
            (append  (dlet-select-deds
                      evaluated-bindings bindings)
                     B))))
      ;; (begin end)
      ((cons 'dseq Deds) 
        (dseq B Deds))
      ;;Propositions
      ((list '$ P) (@prop P))
      ((cons '@prop args) (@prop args))
      ((optima:guard P (prop? P)) F)
      (_ (eval F)))))

(defparameter I #'I)
;;;;
