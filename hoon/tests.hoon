/+  resource-machine
!.
=>  resource-machine
|%
++  use-dec
|=  a=@ud
(dec a)

++  fib
|=  n=@ud
=+  [b=1 a=0]
|-
?:  =(n 0)  a
%=  $
  a  b
  b  (add a b)
  n  (dec n)
==
--
