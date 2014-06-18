;;;; package.lisp


(defpackage #:nucleus-library
  (:use #:cl #:optima)
  (:export :get-var :get-val :subst* :zip))

(defpackage #:patterns
  (:use #:cl #:optima #:nucleus-library)
  (:export :unify :@ #:g :subst-var :F= :sym=))

(defpackage #:omega-dpl
  (:use #:cl #:optima #:nucleus-library #:patterns)
  (:export 
   :prop?
   :define-primitive-method :define-method
   :I :p-value :kernel :matches :is-conditional?
   :*primitive-methods* #:$ #:@ #:@prop #:check-in-base #:top-var
   :assume :in :ex-generalize :from :pick-witness :for :pick-any :specialize
   :forall :exists
   :dseq :suppose-absurd :dlet :with
   #:*B* #:B)
  (:shadowing-import-from #:nucleus-library
                          #:get-var #:get-val #:subst* #:zip)
  (:shadowing-import-from #:optima
                          #:guard
                          #:match))

(defpackage #:dcec
  (:use #:cl #:omega-dpl #:nucleus-library #:patterns)
  (:import-from #:omega-dpl
                :define-primitive-method
                :define-method
                :$ :@ :top-var
                :*B* :@prop :check-in-base
                :B)
  (:shadowing-import-from #:optima
                          #:guard
                          #:match)
  (:shadowing-import-from #:nucleus-library
                          #:get-var #:get-val #:subst* #:zip))

(defpackage #:nucleus
  (:use #:cl #:dcec #:nucleus-library))



(defpackage #:tests
  (:use #:cl #:nucleus #:nucleus-library #:dcec #:omega-dpl #:patterns))
