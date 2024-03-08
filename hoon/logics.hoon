/+  resource-machine
!.
=>  resource-machine
|%
++  balanced-delta
  ^-  resource-logic
  |=  [self=resource tx=resource-transaction]
  ^-  ?
  =(~ delta.tx)
++  counter
  ^-  resource-logic
  |=  [self=resource tx=resource-transaction]
  ^-  ?
  ?.  =(1 (length commitments.tx))
    |
  ?.  =(1 (length nullifiers.tx))
    |
  ?.  =(1 (length delta.tx))
    |
  ?<  ?=(~ delta.tx)
  ?.  sign.i.delta.tx
    |
  &
--