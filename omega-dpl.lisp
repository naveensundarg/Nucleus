
(use-package 'optima)
(defparameter *B* ())
(defparameter *primitive-methods* (make-hash-table))
(defparameter *derived-methods* (make-hash-table))

(defun dpl-error (msg) (error msg))

(defun is-deduction? (F)
  (match F ((cons '! _) t)
	 (otherwise nil)))

(defun body-is-malformed? (body)
  (match body 
    ((cons '! others))))

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
  (if (not (member P B :test #'equalp))
      (error "~a not in the assumption base." P)
      P))


(defun is-primitive-method? (E) 
  (gethash E *primitive-methods*))
(defun is-derived-method? (E) 
  (or (gethash E *derived-methods*)
      (match E 
	((list 'phi _ _) t)
	(otherwise nil))))

(defun is-function? (E)
  (and (fboundp E) (not (macro-function E))))

(defun @prop (x) x)

(defun mapply (m values) (apply (gethash m *primitive-methods*) values))

(defun dapply (m B values) 
  (let ((def (if (symbolp m)
		 (gethash m *derived-methods*)
		 (match m ((list 'phi args ded) (list args ded))))))
    (I (subst* (zip (first def) values) (second def)) B)))

(defun I (F &optional (B *B*))
  (let ((*B* B))
    (match F
      ;Clause 1: ! operator
      ((cons '! (cons E  args))
       (destructuring-bind (Values Bp)
	   (eval-meth-args args *B* #'I)
	 (cond ((is-primitive-method? E)
		(mapply E (cons (append *B* Bp) Values )))
	       ((is-derived-method? E)
		(dapply E (append *B* Bp) Values))
	       (t (error "~a is not a method." E)))))
      ;; Clause 2: assume E in D
      ((list 'assume E 'in D)
       (let* ((P (I E B))
	      (Q (I D (cons P B))))
	 (@prop `(implies ,P ,Q))))
      
      ;;Function applications
      ((guard (cons f args) 
	      (is-function? f)) (apply f (eval-fun-args args *B* #'I)))
      ;; Atoms
      ((not (cons _ _)) F)
      (P P))))

(defparameter I #'I)

;;;;

(defun matches (pat obj) (match obj (pat t) (otherwise nil)))
(defun is-conditional? (P) (matches '(implies _ _) P))
(define-primitive-method claim (P)
  (check-in-base P B))

(define-primitive-method modus-ponens (antecedent implication)
  (flet ((consequent (P) (third P)))
    (check-in-base antecedent B)
    (check-in-base implication B)
    (match  implication
      ((guard imp (and (is-conditional? imp)
                       (matches `(implies ,antecedent _) imp))) (consequent implication)))))

(define-primitive-method left-and (P)
  (check-in-base P B)
  (match P ((list 'and left _) left)))

(define-primitive-method right-and (P)
  (check-in-base P B)
  (match P ((list 'and _ right) right)))

(define-primitive-method and-intro (P Q)
  (check-in-base P B)
  (check-in-base Q B)
  `(and ,P ,Q))


(define-method commutative-and (x)
  (! and-intro (! right-and x) (! left-and x)))




