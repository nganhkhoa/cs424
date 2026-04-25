#import "@preview/touying:0.7.3": *
#import "@preview/showybox:2.0.4": *
#import "@preview/mannot:0.3.1": *

#import themes.simple: *

#show: simple-theme.with(aspect-ratio: "16-9")

= Dynamics of Programming Language

== Effectful Software Contract

Functions can perform side-effects, i.e., IO controls, state management, exceptions, etc.

Many contract systems tried to contraints the effects, usually _ad hoc_ solutions.

Recent research on effect handlers suggests a uniform way to manage effectful code.

This paper integrates contracts and effect handlers.

== Contracts

Guards function inputs and output. A contract between the function and its users.


== Effect handler

Effects are named and invoked like functions.

Invocation varies depends on the handler installed during runtime.

== Effectful Software Contract

Two main questions:

- Contracts for effects called during function execution
- What if contracts also want to invoke an effects?

---

New contruction to contract effects code invoked during function execution.

A contract context handler to handle effects during contract checking.

TODO: add examples

== Summary

#v(5em)
#box($
  & bold("let") sans("pool_c") k =
  (markhl(sans("is_unit"), tag: #<is_unit>, color: #purple) -> markhl(sans("is_any"), tag: #<is_any>, color: #purple))
  inter.sq
  (markhl(sans("has_rem"), tag: #<has_rem>, color: #blue) triangle.stroked.small.r markhl(sans("is_real"), tag: #<is_real>, color: #blue))
  inter.sq
  diamond.stroked.small (markhl(sans("rem_h"), tag: #<rem_h>, color: #green) space markhl(k, tag: #<k>, color: #green))
  \
  #v(2em)\
  & bold("let") sans("f") : sans("pool_c") sans("some_k") = ...

  #annot(<is_unit>, pos: top, dy: -2em)[contract for input of $sans("f")$]
  #annot(<is_any>, pos: top, dy: -1em, dx: 2em)[contract for output of $sans("f")$]
  #annot(<has_rem>, pos: left + bottom, dy: 1em)[contract for effects callable in $sans("f")$]
  #annot(<is_real>, pos: bottom, dy: 2em, dx: -3em)[contract for effects resumption/continue in $sans("f")$]
  #annot(<rem_h>, pos: top, dy: -2em, dx: -3em)[handler for contract-checking code effects]
  #annot(<k>, pos: bottom, dy: 1em, dx: -1em)[handler argument]
$)
