:: Load anoma.hoon into your environemnt as anoma
:: then write .*  name  [0 2] to get the arm
=use-dec =>  anoma
|=  a=@ud
(dec a)

=fib =>  anoma
|=  n=@ud
=+  [b=1 a=0]
|-
?:  =(n 0)  a
%=  $
  a  b
  b  (add a b)
  n  (dec n)
==
