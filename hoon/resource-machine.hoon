/+  anoma
!.
=>  anoma
|%
::  resource machine data structures
::  a resource. see resource machine report
+$  resource
  $:
    logic=resource-logic
    label=@t
    quantity=@
    data=@
    eph=?
    nonce=@
    npk=@
    rseed=@
  ==
::  a transparent resource commitment is the resource.
+$  commitment  @
::  a transparent nullifier is the resource, signed by the nullifier key
+$  nullifier  @
::  a transparent proof is the resource (just the resource logic, really)
::  what is a proof? a proof that the resource logic gives true on this tx.
::  just providing the resource logic is also a proof. the verification
::  is just executing the logic and comparing its result to true.
+$  proof  resource
::  a delta is a signed denominated amount. denom depends on logic and label
::  true = positive. todo: use map instead (when hashes are in)
+$  delta  (list [denom=@ sign=? amount=@])
++  zero-delta  `delta`~
::  a resource transaction. see the resource machine report
+$  resource-transaction  ::  todo: sets instead of lists
  $:
    roots=(list @)
    commitments=(list commitment)
    nullifiers=(list nullifier)
    proofs=(list proof)
    delta=delta  ::  total tx delta
    extra=@
    preference=~ ::  nyi
  ==
::  a resource logic is a function from a transaction to boolean
+$  resource-logic
  $~  =>(~ |=(^ &))
  $-([self=resource tx=resource-transaction] ?)
--