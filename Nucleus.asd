;;;; Nucleus.asd

(asdf:defsystem #:nucleus
  :serial t
  :description "An implementation of an omega denotational proof language."
  :author "Naveen Sundar G. <naveensundarg@gmail.com>"
  :license "GPL"
  :depends-on (#:optima)
  :components ((:file "package")
               (:file "library")
               (:file "patterns")
               (:file "omega-dpl")
               (:file "dcec")
               (:file "./tests/tests")
               (:file "./tests/propositional-tests")
               (:file "./tests/fol-tests")
               (:file "./tests/modal-tests")
               (:file "./snark-20120808r022/snark-system")
               (:file "snark-interface")
               (:file "./subjunctive-reasoning/g")
               (:file "./subjunctive-reasoning/>")
               (:file "./utils/enumerations")))

