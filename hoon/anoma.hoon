::  Simple demo Nock standard library.
!.
=~  %909
~%  %k.909  ~  ~
::  layer 0: version stub (+7)
|%
++  anoma  +
--
::  layer 1: basic arithmetic (+3)
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
::  layer 2: fancy arithmetic (+1)
|%
++  modulo  ::  name this 'mod' and rename 'mod' to 'rem'?
  |_  modulus=@
    ++  reduce
      |=  a=@
      ^-  @
      (mod a modulus)
    ++  congruent
      |=  [a=@ b=@]
      .=  (reduce a)  (reduce b)
    ++  add
      |=  [a=@ b=@]
      ^-  @
      (reduce (^add a b))
    ++  sub
      |=  [a=@ b=@]
      ^-  @
      (reduce (^sub (^add modulus a) (reduce b)))
    ++  mul
      |=  [a=@ b=@]
      ^-  @
      (reduce (^mul a b))
    ++  neg
      |=  a=@
      ^-  @
      (^sub modulus (reduce a))
    ++  inv  ::  only works in prime fields
      !!
    ++  div  ::  only works in prime fields
      |=  [a=@ b=@]
      (mul a (inv b))
  --
--
::  layer n: resources
|%
+$  resource
  $:
    logic=*
    label=@t
    quantity=@
    data=@
    eph=?
    nonce=@
    npk=@
    rseed=@
  ==
+$  commitment  @
+$  nullifier  @
+$  proof  (list resource)
::  true = positive; todo: use map
+$  delta  (list [denom=@ sign=? amount=@])
+$  transaction
  $:
    roots=(list @)
    commitments=(list commitment)
    nullifiers=(list nullifier)
    proofs=(list proof)
    delta=(list delta)
    extra=@
    preference=~
  ==
--
==
