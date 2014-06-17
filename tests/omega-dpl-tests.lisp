 
(defun run-all-tests (&optional (str nil))
  (let ((*reports*  ()))
    (format t "~% ====== RUNNING TESTS ===== ~%" )
    (force-output t)
    (run-propositional-tests str)
    (run-fol-tests str)
    (run-modal-tests str)
    (format t "~% ==== STATS ==== " )
    (format t "~{    ~a~% ~}" (reverse *reports*))
    (format t "~% === TESTS OVER === ~%" )

    (values)))


