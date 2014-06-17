(in-package :nucleus-library)


(defun get-var (binding) (first binding))
(defun get-val (binding) (second binding))


;; Example
;; (subst* '((x 1) (y 2)) '(+ x y)) ==> '(+ 1 2)
(defun subst* (bindings form)
  (reduce
   (lambda (f binding) 
     (subst (get-val binding) (get-var binding) f))
   bindings :initial-value form))


(defun zip (x y) (mapcar (lambda (p q) (list p q)) x y))
