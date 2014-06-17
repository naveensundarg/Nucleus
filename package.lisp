;;;; package.lisp


(defpackage #:nucleus-library
  (:use #:cl #:optima)
  (:export :get-var :get-val :subst* :zip))

(defpackage #:patterns
  (:use #:cl #:optima #:nucleus-library))

(defpackage #:omega-dpl
  (:use #:cl #:optima #:nucleus-library)
  (:export :define-primitive-method :define-method :I)
  (:shadowing-import-from #:nucleus-library
                          #:get-var #:get-val #:subst* #:zip)
  (:shadowing-import-from #:optima
                          #:guard
                          #:match))

(defpackage #:dcec
  (:use #:cl #:omega-dpl #:nucleus-library)
  (:import-from #:omega-dpl
               :define-primitive-method
               :define-method)
  (:shadowing-import-from #:optima
                          #:guard
                          #:match)
  (:shadowing-import-from #:nucleus-library
                          #:get-var #:get-val #:subst* #:zip))

(defpackage #:nucleus
  (:use #:cl #:dcec #:nucleus-library))



