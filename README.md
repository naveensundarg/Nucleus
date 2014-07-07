# Nucleus

This is an implementation of an omega [denotational proof language](http://people.csail.mit.edu/kostas/dpls/) in
Common Lisp.



The system has a first-order modal logic implemented in it.
[Deontic Cognitive Event Calculus](http://www.cs.rpi.edu/~govinn/dcec.pdf)

# Examples

The DPL interpreter is invoked through the function **I**. Some
examples are shown below. Look at the tests for more examples.

Evaluating  
```lisp
(I  '(assume ($ (and P Q)) in 
           (both 
            (right-and ($ (and P Q))) 
            (left-and ($ (and P Q))))))
```
gives us:
```lisp
  (implies (and P Q) (and Q P))
```
