
(defparameter *ql-modules*
  (list "optima"))



(mapcar #'ql:quickload *ql-modules*)


(defparameter *files*
  (list
   "omega-dpl" "omega-dpl-tests"))

(defun compile-and-load (pathname &key (verbose nil))
  (multiple-value-bind (output-pathname warnings-p failure-p)
      (compile-file 
       (merge-pathnames 
        pathname (load-time-value *load-truename*)) :verbose verbose)
    (declare (ignore warnings-p))
    (if failure-p
	(error "Could not compile ~a" pathname)
	(load output-pathname))))


(map nil 'compile-and-load *files*)

