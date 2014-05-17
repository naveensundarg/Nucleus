 

(defun run-all-tests (&optional (str nil))
  (format t "~% --- RUNNING TESTS --- ~%" )
  (force-output t)
  (run-propositional-tests str))
