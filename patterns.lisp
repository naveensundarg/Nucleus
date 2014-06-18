(in-package :patterns)

(defun variablep (sym)
  (if (symbolp sym)  
      (let ((name (symbol-name sym)))
	(and
	 (equal "?" (subseq name 0 1))
	 (or
	  (equalp "_" (subseq name 1 2))
	  (alpha-char-p (char name 1)))))))
(defun quantifier (quantifiedF)
  (optima:match quantifiedF 
    ((list 'forall _ _) 'forall)
    ((list 'exists _ _) 'exists)))
(defun subst-var (var term F &optional (bound nil))
  (optima:match F 
    ((list (or 'forall 'exists) vars K) 
     (list (quantifier F)  vars (subst-var var term K (append bound vars))))
    ((cons head args)
     (cons head (mapcar (lambda (arg) (subst-var var term arg bound)) args)))
    (otherwise
     (if (and   (equalp F var) (not (member F bound)))
         term F))))

(defun sym= (x y)
  (and (symbolp x) (symbolp y) (equalp (symbol-name x) (symbol-name y))))
(defun F= (F1 F2 &key (bound1 nil) (bound2 nil))
  (optima:match (list F1 F2) 
    ((or (list (list 'omega-dpl::forall vars1 K1) (list 'omega-dpl::forall vars2 K2))
         (list (list 'omega-dpl::exists vars1 K1) (list 'omega-dpl::exists vars2 K2)))
     (F= K1 K2 :bound1 (append bound1 vars1) :bound2 (append bound2 vars2)))
    ((list (cons head1 args1) (cons head2 args2))
     (every (complement #'null)
      (cons (F= head1 head2 :bound1 bound1 :bound2 bound2)  
            (mapcar (lambda (a1 a2) (F= a1 a2 :bound1 bound1 :bound2 bound2)) 
                    args1 args2))))
    (otherwise
     (or
      (if (and (member F1 bound1) (member F2 bound2))
       (= (position F1 bound1) (position F2 bound2)))
      (sym= F1 F2)))))

(defun wildcardvarp (sym)
  (and (variablep sym) 
       (equalp "_" (subseq (symbol-name sym) 1 2))))

(defun get-var (binding) (first binding))
(defun get-val (binding) (second binding))

(defun @ (var bindings) (second (assoc var bindings)))

(defun bindings-consistent? (bindings)
  "Given a list of bindings, checks whether it is consistent.
   If it is consistent, returns a canoncial form without any duplicates.
   If it is not consistent, throws an error."
  (reduce (lambda (x y)
	    (let ((present? (member (get-var y) x :key #'first :test #'equalp))) 
	      (if present?
		  (if (not (equalp (get-val (first present?)) (get-val y))) 
		      (error "Inconsistent-bindings ~a ~a" y x)
		      x)
		  (cons y x))))
	  bindings :initial-value nil))

(defun unify (x y &key (rest nil))
  (cond
    ((variablep x) (list (list x y)))
    ((variablep y) (list (list y x)))
    ((and (atom x) (atom y)) 
     (if (eql x y)
	 () (error "Atoms ~a and ~a don't unify." x y)))
    ((and (atom x) (listp y) (not rest))
     (error "Cannot unify atom ~a with cons ~a" x y))
    ((and (atom y) (listp x) (not rest))
     (error "Cannot unify atom ~a with cons ~a" y x))
    ((and rest (atom x) (variablep x) (listp y))
     (list (list x y)))
    ((and rest (atom y) (variablep y) (listp x))
     (list (list y x)))
    (t 
     (let ((bindings
	    (append (unify (car x) (car y))
		    (cond 
		      ((equalp '&rest (second x))
		       (unify (caddr x) (cdr y) :rest t))
		      ((equalp '&rest (second y))
		       (unify (cdr x) (caddr y) :rest t))
		      (t (unify (cdr x) (cdr y)))))))
       (bindings-consistent? bindings)))))

(defun value (var bindings)
  (second (assoc var bindings)))

(defun get-let-list (pattern value)
  (remove nil (mapcar (lambda (binding)
			(if (wildcardvarp (get-var binding))
			    nil
			    (list (get-var binding)
				  (get-val binding))))
		      (unify pattern value))))

;; Example usage of plet
;; (plet (or ?p ?q) '(and (if a b) (and r w)) &rest body)
(defmacro plet (pattern value &body body)
  `(let* ((bindings (get-let-list ',pattern ,value) )
	  (nbody  (reduce
			 (lambda (f binding) 
			   (subst (list 'quote (get-val binding)) (get-var binding) f))
			 bindings :initial-value (list 'progn ',@body))))
     (eval nbody)))


;; Example usage of plet*
;; (defun foo (x)
;; (plet* x 
;; 	  ((sum ?x ?y) (+ ?x ?y))
;; 	  ((mult ?x ?y) (* ?x ?y))))
(defmacro plet* (obj &rest conditions)
  `(block :top 
     ,@(mapcar (lambda (condition)
		 `(handler-case
		      (return-from :top 
			(plet ,(first condition) ,obj ,@(rest condition)))
		    (simple-error (condition) (declare (ignore condition)))))
	       conditions)))

(defun sunify (p obj)
  (handler-case (unify p obj)
      (simple-error (condition) (declare (ignore condition)))))

(defun variables (P)
  (cond ((variablep P)(list P))
        ((atom P) nil)
        (t (remove-duplicates (apply #'append (mapcar #'variables P))
                            :test #'equalp))))

(defun freevars (P)
  (let ((all-vars (variables P)))
    (optima:match P
      ((or (list 'exists vars Q) (list 'forall vars Q)) 
       (set-difference all-vars vars :test #'equalp))
      (_ all-vars))))
