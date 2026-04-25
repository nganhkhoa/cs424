#import "@preview/mannot:0.3.2": *

$
$

#box($
  & bold("let") sans("pool_c") k =
  (markhl(sans("is_unit"), tag: #<unit>, color: #purple) -> markhl(sans("is_any"), tag: #<any>, color: #purple))
  inter.sq
  (markhl(sans("has_rem"), tag: #<has_rem>, color: #blue) triangle.stroked.small.r markhl(sans("is_real"), tag: #<real>, color: #blue))
  inter.sq
  diamond.stroked.small (markhl(sans("rem_h"), tag: #<rem_h>, color: #green) space markhl(k, tag: #<k_val>, color: #green))
  \
  #v(2em)\
  & bold("let") sans("f") : sans("pool_c") sans("some_k") = ...

  #annot(<unit>, pos: top, dy: -2em)[contract for input of $sans("f")$]
  #annot(<any>, pos: top, dy: -1em, dx: 2em)[contract for output of $sans("f")$]
  #annot(<has_rem>, pos: left + bottom, dy: 1em)[contract for effects callable in $sans("f")$]
  #annot(<real>, pos: bottom, dy: 2em)[contract for effects resumption/continue in $sans("f")$]
  #annot(<rem_h>, pos: top, dy: -2em)[handler for contract-checking code effects]
  #annot(<k_val>, pos: bottom, dy: 1em, dx: 2em)[handler argument to contract-checking code effects]
$)
