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
    commitments=(set commitment)   ::  commitment set
    nullifiers=(set nullifier)     ::  nullifier set
    self-tag=@                     ::  exactly one commitment or nullifier
    other-public=*                 ::  some other public inputs
  ==
+$  private-inputs
  $:
    committed-resources=(set resource)   ::  committed resource set
    nullified-resources=(set resource)   ::  nullified resource set
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
    app-data=~
  ==
  $:
    commitments=(set commitment)   ::  commitment set
    nullifiers=(set nullifier)     ::  nullifier set
    proofs=(set proof)             ::  proof set
    app-data=(map @ [* ?])       ::  hash-addressed data with storage criteria
  ==
+$  cm-root  @                     ::  commitment set root
+$  resource-kind  @
+$  delta  (map k=resource-kind v=@s)     ::  delta (opaque)
+$  transaction
  $~  :*
    roots=~
    actions=~
    delta=*delta
    delta-proof=%delta
  ==
  $:
    roots=(set cm-root)    ::  root set for spent resources
    actions=(set action)   ::  action set
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
  ~/  %kind
  |=  =resource
  ^-  resource-kind
  (shax (jam [label.resource logic.resource]))
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
  (malt ~[[(kind resource) (sun quantity.resource)]])
++  action-delta
  ~/  %action-delta
  |=  =action
  =+  c=%action-delta
  ^-  delta
  !!
++  make-delta  ::  make delta from actions (to make a transaction)
  ~/  %make-delta
  |=  actions=(set action)
  =+  c=%make-delta
  ^-  delta
  !!
++  prove-delta  ::  prove delta, trivially
  |=  *
  %delta
++  zero-delta  ::  the value of the zero delta, for convenience
  ~
++  trm-compliance-key
  ~/  %trm-compliance-key
  |=  [nfs=(list nullifier) cms=(list commitment) delta=@]
  =+  c=%trm-compliance-key
  ^-  ?
  !!
++  trm-delta-key
  ~/  %trm-delta-key
  |=  [delta=@ expected=@]
  =+  c=%trm-delta-key
  ^-  ?
  =(delta expected)
--
