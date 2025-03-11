/+  anoma
!.
=>  anoma
|%
::  type definitions
+$  json                            ::  ordered json value for cairo
  $@  ~                             ::  null
  $%  [%a p=(list json)]            ::  array
      [%b p=?]                      ::  boolean
      [%o p=(list (pair @t json))]  ::  object
      [%n p=@ta]                    ::  number
      [%s p=@t]                     ::  string
  ==                                ::
+$  t-resource
  $~  :*
    logicref=42.693
    labelref=`@u`2
    valueref=`@u`2
    quantity=`@u`1
    isephemeral=|
    nonce=*@I
    nullifierkeycommitment=*@I
    randseed=%fake
  ==
  $:
    logicref=@u                ::  jam of logic
    labelref=@u                ::  label reference
    valueref=@u                ::  value reference
    quantity=@u                ::  quanitity
    isephemeral=?              ::  ephemerality flag
    nonce=@I                   ::  nonce
    nullifierkeycommitment=@I  ::  nullifer key
    randseed=%fake             ::  fake random seed field
  ==
+$  t-compliance-unit
  $~  :*
    proof=*t-proof
    instance=*t-compliance-instance
    vk=3.561.077.109.446.357.667.049.832.284.909.642.655.530.995.937.367.874.908.225
  ==
  $:
    proof=t-proof                   ::  compliance proof
    instance=t-compliance-instance  ::  compliance instance
    vk=@                            ::  verification key
  ==
+$  t-compliance-instance
  $~  :*
    consumed=~
    created=~
    unit-delta=`@u`2
  ==
  $:
    consumed=(list [t-nullifier @ @])     ::  nullifiers
    created=(list (pair t-commitment @))  ::  commitments
    unit-delta=@                          ::  unit delta
  ==
+$  t-action
  $~  :*
    created=~
    consumed=~
    resource-logic-proofs=~
    compliance-units=~
    app-data=~
  ==
  $:
    created=(list t-commitment)                                   ::  commitment list
    consumed=(list t-nullifier)                                   ::  nullifier list
    resource-logic-proofs=(map t-tag (pair @ t-compliance-unit))  ::  proof set
    compliance-units=(list t-compliance-unit)                     ::  compliance units
    app-data=(map t-tag (list (pair @ ?)))                        ::  tags with blobs
  ==
+$  t-transaction
  $~  :*
    roots=~
    actions=~
    delta-proof=*t-proof
  ==
  $:
    roots=(set @)           ::  root set for spent resources
    actions=(set t-action)  ::  action set
    delta-proof=t-proof     ::  delta proof (trivial)
  ==
+$  t-proof  %fake
+$  t-tag  $?(t-commitment t-nullifier)
+$  t-commitment  @
+$  t-nullifier  @
+$  t-root  @
++  commit  ::  commit to a resource
  |=  =t-resource
  ^-  t-commitment
  (~(cat block 3) 'CM_' (jam t-resource))
++  nullify  ::  nullify a resource
  |=  =t-resource
  ^-  t-nullifier
  (~(cat block 3) 'NF_' (jam t-resource))
++  is-commitment  ::  check whether an atom is a commitment
  |=  a=@
  ^-  @
  =('CM_' (~(end block 3) 3 a))
++  is-nullifier  ::  check whether an atom is a nullifier
  |=  a=@
  ^-  @
  =('NF_' (~(end block 3) 3 a))
++  kind
  ~/  %kind
  |=  =t-resource
  ^-  @I
  (shax (jam [labelref.t-resource logicref.t-resource]))
++  delta-add
  ~/  %delta-add
  |=  [d1=@ d2=@]
  =+  c=%delta-add
  ^-  @
  !!
++  delta-sub
  ~/  %delta-sub
  |=  [d1=@ d2=@]
  =+  c=%delta-sub
  ^-  @
  !!
++  action-delta
  ~/  %action-delta
  |=  =t-action
  =+  c=%action-delta
  ^-  @
  !!
++  compliance-unit-delta
  ~/  %compliance-unit-delta
  |=  =t-compliance-unit
  =+  c=%compliance-unit-delta
  ^-  @
  !!
++  make-delta  ::  make delta from actions
  ~/  %make-delta
  |=  actions=(set t-action)
  =+  c=%make-delta
  ^-  @
  !!
++  action-create  ::  create interface for actions
  ~/  %action-create
  |=  [consumed=(list [@I t-resource t-root]) created=(list t-resource) data=(map t-tag (list (pair @ ?)))]
  =+  c=%action-create
  ^-  t-action
  !!
++  trm-compliance-key
  ~/  %trm-compliance-key
  |=  [nfs=(list t-nullifier) cms=(list t-commitment) delta=@]
  =+  c=%trm-compliance-key
  ^-  ?
  !!
++  trm-delta-key
  ~/  %trm-delta-key
  |=  [delta=@ expected=@]
  =+  c=%trm-delta-key
  ^-  ?
  =(delta expected)
++  zero-delta  ::  the value of the zero delta, for convenience
  ~
--
