::  Simple demo Nock standard library.
!.
=~  %909
~%  %k.909  ~  ~
::  layer 0: version stub (+3)
|%
++  anoma  +
--
::  layer 1: basic arithmetic (+1)
~%  %one  +  ~
|%
++  dec  ::  +342
  ~/  %dec
  |=  a=@
  ?<  =(0 a)
  =|  b=@
  |-  ^-  @
  ?:  =(a +(b))  b
  $(b +(b))
++  add  ::  +20
  ~/  %add
  |=  [a=@ b=@]
  ^-  @
  ?:  =(0 a)  b
  $(a (dec a), b +(b))
++  sub  ::  +47
  ~/  %sub
  |=  [a=@ b=@]
  ^-  @
  ?:  =(0 b)  a
  $(a (dec a), b (dec b))
++  lth  ::  +343
  ~/  %lth
  |=  [a=@ b=@]
  ^-  ?
  ?&  !=(a b)
      |-
      ?|  =(0 a)
          ?&  !=(0 b)
              $(a (dec a), b (dec b))
  ==  ==  ==
++  lte  ::  +84
  ~/  %lte
  |=  [a=@ b=@]
  ^-  ?
  |(=(a b) (lth a b))
++  gth  ::  +43
  ~/  %gth
  |=  [a=@ b=@]
  ^-  ?
  !(lte a b)
++  gte  ::  +22
  ~/  %gte
  |=  [a=@ b=@]
  ^-  ?
  !(lth a b)
++  mul  ::  +4
  ~/  %mul
  |:  [a=`@`1 b=`@`1]
  =|  c=@
  |-  ^-  @
  ?:  =(0 a)  c
  $(a (dec a), c (add b c))
++  div  ::  +170
  ~/  %div
  |:  [a=`@`1 b=`@`1]
  ?<  =(0 b)
  =|  c=@
  |-  ^-  @
  ?:  (lth a b)  c
  $(a (sub a b), c +(c))
++  mod  ::  +46
  ~/  %mod
  |:  [a=`@`1 b=`@`1]
  ^-  @
  ?<  =(0 b)
  (sub a (mul b (div a b)))
--
::  layer 2: data structures, HOFs
~%  %two  +  ~
|%
::  some types
+$  mold
  $~(* $-(* *))
+$  gate
  $-(* *)
++  trap
  |$  [product]
  _|?($:product)
++  unit
  |$  [item]
  $@(~ [~ u=item])
++  each
  |$  [left right]
  $%  [%| p=left]
      [%& p=right]
  ==
++  list
  |$  [item]
  $@(~ [i=item t=(list item)])
++  nonempty-list
  |$  [item]
  [i=item t=(list item)]
++  tree
  |$  [node]
  $@(~ [n=node l=(tree node) r=(tree node)])
::  utilities on some of these types
++  length
  |*  l=(list)
  ?~  l
    0
  +($(l t.l))
::  HOFs on some of these types
++  curry
  |*  [a=$-(^ *) b=*]
  =+  c=+<+.a
  |@
  ++  $
    (a b c)
  --
++  turn  ::  map over list
  |*  [a=(list) b=gate]
  ^-  (list _?>(?=(^ a) (b i.a)))
  |-
  ?~  a  ~
  [i=(b i.a) t=$(a t.a)]
++  foldr
  |*  [a=(list) b=_=>(~ |=([* *] +<+))]
  |-  ^+  ,.+<+.b
  ?~  a
    +<+.b
  (b i.a $(a t.a))
--
::  layer 3: fancy arithmetic
~%  %three  +  ~
|%
++  pow
  ~/  %pow
  |=  [a=@ b=@]
  ^-  @
  ?:  =(0 b)
    1
  (mul a $(b (dec b)))
++  modulo  ::  name this 'mod' and rename 'mod' to 'rem'?
  |_  modulus=@
  ++  reduce
    |=  a=@
    ^-  @
    (mod a modulus)
  ++  congruent
    |=  [a=@ b=@]
    =((reduce a) (reduce b))
  ++  add
    |=  [a=@ b=@]
    ^-  @
    (reduce (^add a b))
  ++  sub
    |=  [a=@ b=@]
    ^-  @
    (reduce (^sub (^add modulus a) (reduce b)))
  ++  mul
    |:  [a=`@`1 b=`@`1]
    ^-  @
    (reduce (^mul a b))
  ++  pow
    |=  [a=@ b=@]
    ^-  @
    (reduce (^pow a b))
  ++  neg
    |=  a=@
    ^-  @
    (^sub modulus (reduce a))
  ++  inv  ::  only works in prime fields
    |=  a=@
    ^-  @
    !!
  ++  div  ::  only works in prime fields
    |:  [a=`@`1 b=`@`1]
    ^-  @
    (mul a (inv b))
  --
--
::  layer 4: bits and bytes
~%  %four  +  ~
|%
++  bex  ::  2^a
  |=  a=@
  ^-  @
  ?:  =(0 a)  1
  (mul 2 $(a (dec a)))
++  block  ::  better name?
  |_  block-size=@  ::  exponent of 2, i.e. size 3 = 8 bits
  ++  bits
    (bex block-size)
  ++  modulus
    (bex (bex block-size))
  ++  lsh
    |=  [count=@ value=@]
    ^-  @
    ::  lsh(n, a) = a * 2^(bits to shift)
    (mul value (bex (mul bits count)))
  ++  rsh
    |=  [count=@ value=@]
    ^-  @
    ::  rsh(n, a) = a / 2^(bits to shift)
    (div value (bex (mul bits count)))
  ++  end  ::  least significant blocks
    |=  [count=@ value=@]
    ^-  @
    (mod value (bex (mul bits count)))
  ++  cut  ::  slice an array
    |=  [[offset=@ length=@] value=@]
    ^-  @
    (end length (rsh offset value))
  ++  cat  ::  lengthless concatenate, lsb-first
    |=  [fst=@ snd=@]
    ^-  @
    (add (lsh (met fst) snd) fst)
  ++  fil  ::  fill with repeating
    |=  [count=@ value=@]
    ^-  @
    =|  n=@
    =.  value  (reduce value)
    =/  result  value
    |-
    ?:  =(n count)
      (rsh 1 result)
    $(result (add value (lsh 1 result)), n +(n))
  ++  reduce  ::  shortcut to reduce modulo block
    |=  a=@
    ^-  @
    (end 1 a)
  ++  met  ::  measure in current block size
    |=  a=@
    ^-  @
    =|  result=@
    |-
    ?:  =(0 a)  result
    $(a (rsh 1 a), result +(result))
  ++  inv  ::  invert all bits mod block size
    |=  a=@
    ^-  @
    (sub (dec modulus) (reduce a))
  ++  w-add  ::  wrapping addition mod block size
    |=  [a=@ b=@]
    ^-  @
    (reduce (add a b))
  ++  twos-complement  ::  two's complement mod block size
    |=  a=@
    ^-  @
    (w-add (inv a) 1)
  --
++  xeb  ::  log_2(a) + 1
  ::  NB: not an inverse to bex
  ::  this is "number of bits required"
  |=  a=@
  ^-  @
  (~(met block 0) a)
--
==
