/+  anoma
!.
=>  anoma
|%
::  type definitions
+$  resource
  $~  :*
    label=*@t
    logic=*resource-logic
    ephemeral=|
    quantity=`@u`1
    data=*[@u @]
    nullifier-key=*@I
    nonce=*@I
    rseed=%fake
  ==
  $:
    label=@t              ::  the label; some binary, @t for convenience
    logic=resource-logic  ::  the logic predicate
    ephemeral=?           ::  the ephemerality flag
    quantity=@u           ::  quantity; some nonnegative integer
    data=[len=@u val=@]   ::  arbitrary data; see lengthy comment in
                          ::  resource.ex. words mean things
    nullifier-key=@I      ::  npk; 256 bits
    nonce=@I              ::  nonce for uniqueness; 256 bits
    rseed=%fake           ::  useless field. meaningless value
  ==
+$  commitment  @
+$  nullifier  @
+$  public-inputs
  $:
    commitments=(list commitment)  ::  commitment set
    nullifiers=(list nullifier)    ::  nullifier set
    self-tag=@                     ::  exactly one commitment or nullifier
    other-public=*                 ::  some other public inputs
  ==
+$  private-inputs
  $:
    committed-resources=(list resource)  ::  committed resource set
    nullified-resources=(list resource)  ::  nullified resource set
    other-private=*                      ::  some other private inputs
  ==
+$  resource-logic
  $~  =>(~ |=(* &))
  $-([public-inputs private-inputs] ?)
+$  logic-proof
  [resource=resource inputs=[public-inputs private-inputs]]
+$  compliance-proof  %compliance
+$  proof  ?(compliance-proof logic-proof)
+$  action
  $~  :*
    commitments=~
    nullifiers=~
    proofs=~
    app-data=**
  ==
  $:
    commitments=(list commitment)  ::  commitment set
    nullifiers=(list nullifier)    ::  nullifier set
    proofs=(list proof)            ::  proof set
    app-data=*                     ::  arbitrary data
  ==
+$  cm-root  @                     ::  commitment set root
+$  resource-kind
  [label=@t logic=resource-logic]
+$  delta-element
  [k=resource-kind v=@s]
+$  delta  (list delta-element)    ::  delta (opaque)
+$  transaction
  $~  :*
    roots=~
    actions=~
    delta=*delta
    delta-proof=%delta
  ==
  $:
    roots=(list cm-root)   ::  root set for spent resources
    actions=(list action)  ::  action set
    delta=delta            ::  delta (opaque)
    delta-proof=%delta     ::  delta proof (trivial)
  ==
::  provided functions
++  private  ::  DO NOT USE!
  |%
  ++  uncommit
    |=  =commitment
    ;;  resource
    (cue (~(rsh block 3) 3 commitment))
  ++  unnullify
    |=  =nullifier
    ;;  resource
    (cue (~(rsh block 3) 3 nullifier))
  --
++  commit  ::  commit to a resource
  |=  =resource
  ^-  commitment
  (~(cat block 3) 'CM_' (jam resource))
++  nullify  ::  nullify a resource
  |=  =resource
  ^-  nullifier
  (~(cat block 3) 'NF_' (jam resource))
++  is-commitment  ::  check whether an atom is a commitment
  |=  a=@
  ^-  @
  =('CM_' (~(end block 3) 3 a))
++  is-nullifier  ::  check whether an atom is a nullifier
  |=  a=@
  ^-  @
  =('NF_' (~(end block 3) 3 a))
++  kind
  |=  =resource
  ^-  resource-kind
  [label.resource logic.resource]
++  prove-logic  ::  prove a resource logic given all inputs
  |=  =logic-proof
  (jam logic-proof)
++  prove-action  ::  prove action compliance, trivially
  |=  *
  %compliance
++  delta-add
  ~/  %delta-add
  |=  [d1=delta d2=delta]
  =+  c=%delta-add
  ^-  delta
  !!
++  delta-sub
  ~/  %delta-sub
  |=  [d1=delta d2=delta]
  =+  c=%delta-sub
  ^-  delta
  !!
++  resource-delta
  |=  =resource
  ^-  delta
  ~[[(kind resource) (sun quantity.resource)]]
++  action-delta
  ~/  %action-delta
  |=  =action
  =+  c=%action-delta
  ^-  delta
  !!
++  make-delta  ::  make delta from actions (to make a transaction)
  ~/  %make-delta
  |=  actions=(list action)
  =+  c=%make-delta
  ^-  delta
  !!
++  prove-delta  ::  prove delta, trivially
  |=  *
  %delta
++  zero-delta  ::  the value of the zero delta, for convenience
  ~
--
