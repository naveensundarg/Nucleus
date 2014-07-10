

(defun create-signature () ())
(defun add-to-signature (signature &key name output (inputs nil))
  (cons  (list name output inputs) signature))

(defun name (s) (first s))
(defun outputs (s) (second s))
(defun inputs (s) (third s))

(defun get-leaves (signature) (remove nil 
                                      (mapcar (lambda (s) 
                                                (if  (= 0 (length (inputs s))) s nil))
                                              signature)))

(defun cartesian-product (sets)
  (if ( < (length sets) 2) (mapcar (lambda (x) (list x)) (first sets))
      (let ((rst (cartesian-product (rest sets))))
        (apply #'append 
               (mapcar (lambda (x) (mapcar (lambda (y) (cons x y)) rst)) 
                       (first sets))))))

(defun multiply (term-sig terms)
  (let ((inp-sorts (inputs term-sig)))
    (mapcar (lambda (inputs) (list `(,(name term-sig) ,@(mapcar #'name inputs)) (outputs term-sig)))
     (cartesian-product
      (mapcar 
       (lambda (inp-sort) 
         (remove nil
                 (mapcar (lambda (term)
                           (if  (equalp inp-sort (outputs term)) term nil)) 
                         terms)))
       inp-sorts)))))

(defun multiply-all (signature terms)
  (let ((compounds 
          (set-difference signature (get-leaves signature)
                          :test #'equalp)))
    (apply #'append (mapcar (lambda (term-sig) (multiply term-sig terms))
                              compounds)))) 

(defun generate-at-depth (signature depth)
  (if (= depth 0)
      (mapcar #'butlast (get-leaves signature))
      (remove nil  (multiply-all signature (generate-at-depth signature (1- depth))))))

(defun generate-int (signature currdepth maxdepth)
  (if (< maxdepth currdepth)
      nil
      (let ((curr-level-terms (generate-at-depth signature currdepth)))
        (if curr-level-terms
            (append  curr-level-terms (generate-int signature (1+ currdepth) maxdepth))))))

(defun generate (signature depth)
  (mapcar #'name 
          (generate-int signature 0 depth)))
