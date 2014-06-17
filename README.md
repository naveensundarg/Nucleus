# Nucleus

This is an implementation of an omega denotational proof language in
Common Lisp.

[DPL](http://people.csail.mit.edu/kostas/dpls/)

The system has a first-order modal logic implemented in it.
[Deontic Cognitive Event Calculus](http://www.cs.rpi.edu/~govinn/dcec.pdf)

# Examples

The DPL interpreter is invoked through the function **I**. Some
examples are shown below. Look at the tests for more examples.

```lisp
(I  '(both (right-and ($ (and P Q))) (left-and ($ (and P Q)))))
```
