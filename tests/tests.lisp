(in-package :dcec)

(defparameter *reports* nil)
(defparameter *verbose* nil)
(defparameter *count* 0)

(defparameter *line* (let ((line "")) 
                       (loop for i from 1 to 50 do (setf line (concatenate
                       'string line "-")))
                       line))
(defun run-all-tests (&optional (str nil))
  (let ((*reports* nil)
        (*count* 0)) 
      (dcec::run-propositional-tests str)
      (dcec::run-fol-tests str)
      (dcec::run-modal-tests str)
      (format t *line*)
      (format t "~{~a~%~}" (reverse *reports*))
      (format t *line*)))
