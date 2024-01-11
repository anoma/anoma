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
::  layer 2: fancy arithmetic
~%  %two  +  ~
|%
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
::  layer 3: bits and bytes
~%  %three  +  ~
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
  ++  can  ::  assemble from list (need list)
    !!
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
::  layer 4: cryptography
~%  %four  +  ~
|%
::  todo: move this stuff
++  unit
  |$  [item]
  $@(~ [~ u=item])
++  need
  |*  a=(unit)
  ?~  a  !!
  u.a
++  list
  |$  [item]
  $@(~ [i=item t=(list item)])
++  length
  |*  l=(list)
  ^-  @
  ?~  l  0
  +($(l t.l))
++  turn
  |*  l=(list) f=$-(* *)
  ?~  l  ~
  [(f i.l) $(l t.l)]
++  foldr
  |*  l=(list) f=$-(* *)
  ?~  l  +<+.f
  (b i.l $(l t.l))
++  cut  ::  compatibility shim
  |=  *  !!
++  can  ::  compatibility shim
  |=  *  !!
++  dis  ::  compatibility shim
  |=  *  !!
++  lsh  ::  compatibility shim
  |=  *  !!
++  rsh  ::  compatibility shim
  |=  *  !!
++  met  ::  compatibility shim
  |=  *  !!
++  fil  ::  compatibility shim
  |=  *  !!
++  con  ::  compatibility shim
  |=  *  !!
++  end  ::  compatibility shim
  |=  *  !!
++  curt  ::  compatibility shim
  |=  *  !!
++  shal  ::  compatibility shim
  |=  *  !!
++  shaz  ::  compatibility shim
  |=  *  !!
++  fo  ::  compatibility shim (modulo prime)
  |_  a=@
  ++  dif  ~(sub modulo a)
  ++  fra  ~(div modulo a)
  ++  exp  |=(* !!)
  ++  sum  ~(add modulo a)
  ++  pro  ~(mul modulo a)
  ++  sit  ~(reduce modulo a)
  ++  inv  ~(inv modulo a)
  --
++  fe  ::  compatibility shim (modulo bloq)
  |_  a=@
  ++  dif
    !!
  ++  inv
    !!
  ++  net
    !!
  ++  out
    !!
  ++  rol
    !!
  ++  ror
    !!
  ++  sum
    !!
  ++  sit
    !!
  --
::  various SHA hashes
++  sha-1  !!
++  sha-2  !!
++  sha-3  !!
::  sha2-512
++  shal
  ~/  %shal
  |=  [len=@ ruz=@]  ^-  @
  =>  .(ruz (cut 3 [0 len] ruz))
  =+  [few==>(fe .(a 6)) wac=|=([a=@ b=@] (cut 6 [a 1] b))]
  =+  [sum=sum.few ror=ror.few net=net.few inv=inv.few]
  =+  ral=(lsh [0 3] len)
  =+  ^=  ful
      %+  can  0
      :~  [ral ruz]
          [8 128]
          [(mod (sub 1.920 (mod (add 8 ral) 1.024)) 1.024) 0]
          [128 (~(net fe 7) ral)]
      ==
  =+  lex=(met 10 ful)
  =+  ^=  kbx  0x6c44.198c.4a47.5817.5fcb.6fab.3ad6.faec.
                 597f.299c.fc65.7e2a.4cc5.d4be.cb3e.42b6.
                 431d.67c4.9c10.0d4c.3c9e.be0a.15c9.bebc.
                 32ca.ab7b.40c7.2493.28db.77f5.2304.7d84.
                 1b71.0b35.131c.471b.113f.9804.bef9.0dae.
                 0a63.7dc5.a2c8.98a6.06f0.67aa.7217.6fba.
                 f57d.4f7f.ee6e.d178.eada.7dd6.cde0.eb1e.
                 d186.b8c7.21c0.c207.ca27.3ece.ea26.619c.
                 c671.78f2.e372.532b.bef9.a3f7.b2c6.7915.
                 a450.6ceb.de82.bde9.90be.fffa.2363.1e28.
                 8cc7.0208.1a64.39ec.84c8.7814.a1f0.ab72.
                 78a5.636f.4317.2f60.748f.82ee.5def.b2fc.
                 682e.6ff3.d6b2.b8a3.5b9c.ca4f.7763.e373.
                 4ed8.aa4a.e341.8acb.391c.0cb3.c5c9.5a63.
                 34b0.bcb5.e19b.48a8.2748.774c.df8e.eb99.
                 1e37.6c08.5141.ab53.19a4.c116.b8d2.d0c8.
                 106a.a070.32bb.d1b8.f40e.3585.5771.202a.
                 d699.0624.5565.a910.d192.e819.d6ef.5218.
                 c76c.51a3.0654.be30.c24b.8b70.d0f8.9791.
                 a81a.664b.bc42.3001.a2bf.e8a1.4cf1.0364.
                 9272.2c85.1482.353b.81c2.c92e.47ed.aee6.
                 766a.0abb.3c77.b2a8.650a.7354.8baf.63de.
                 5338.0d13.9d95.b3df.4d2c.6dfc.5ac4.2aed.
                 2e1b.2138.5c26.c926.27b7.0a85.46d2.2ffc.
                 1429.2967.0a0e.6e70.06ca.6351.e003.826f.
                 d5a7.9147.930a.a725.c6e0.0bf3.3da8.8fc2.
                 bf59.7fc7.beef.0ee4.b003.27c8.98fb.213f.
                 a831.c66d.2db4.3210.983e.5152.ee66.dfab.
                 76f9.88da.8311.53b5.5cb0.a9dc.bd41.fbd4.
                 4a74.84aa.6ea6.e483.2de9.2c6f.592b.0275.
                 240c.a1cc.77ac.9c65.0fc1.9dc6.8b8c.d5b5.
                 efbe.4786.384f.25e3.e49b.69c1.9ef1.4ad2.
                 c19b.f174.cf69.2694.9bdc.06a7.25c7.1235.
                 80de.b1fe.3b16.96b1.72be.5d74.f27b.896f.
                 550c.7dc3.d5ff.b4e2.2431.85be.4ee4.b28c.
                 1283.5b01.4570.6fbe.d807.aa98.a303.0242.
                 ab1c.5ed5.da6d.8118.923f.82a4.af19.4f9b.
                 59f1.11f1.b605.d019.3956.c25b.f348.b538.
                 e9b5.dba5.8189.dbbc.b5c0.fbcf.ec4d.3b2f.
                 7137.4491.23ef.65cd.428a.2f98.d728.ae22
  =+  ^=  hax  0x5be0.cd19.137e.2179.1f83.d9ab.fb41.bd6b.
                 9b05.688c.2b3e.6c1f.510e.527f.ade6.82d1.
                 a54f.f53a.5f1d.36f1.3c6e.f372.fe94.f82b.
                 bb67.ae85.84ca.a73b.6a09.e667.f3bc.c908
  =+  i=0
  |-  ^-  @
  ?:  =(i lex)
    (run 6 hax net)
  =+  ^=  wox
      =+  dux=(cut 10 [i 1] ful)
      =+  wox=(run 6 dux net)
      =+  j=16
      |-  ^-  @
      ?:  =(80 j)
        wox
      =+  :*  l=(wac (sub j 15) wox)
              m=(wac (sub j 2) wox)
              n=(wac (sub j 16) wox)
              o=(wac (sub j 7) wox)
          ==
      =+  x=:(mix (ror 0 1 l) (ror 0 8 l) (rsh [0 7] l))
      =+  y=:(mix (ror 0 19 m) (ror 0 61 m) (rsh [0 6] m))
      =+  z=:(sum n x o y)
      $(wox (con (lsh [6 j] z) wox), j +(j))
  =+  j=0
  =+  :*  a=(wac 0 hax)
          b=(wac 1 hax)
          c=(wac 2 hax)
          d=(wac 3 hax)
          e=(wac 4 hax)
          f=(wac 5 hax)
          g=(wac 6 hax)
          h=(wac 7 hax)
      ==
  |-  ^-  @
  ?:  =(80 j)
    %=  ^$
      i  +(i)
      hax  %+  rep  6
           :~  (sum a (wac 0 hax))
               (sum b (wac 1 hax))
               (sum c (wac 2 hax))
               (sum d (wac 3 hax))
               (sum e (wac 4 hax))
               (sum f (wac 5 hax))
               (sum g (wac 6 hax))
               (sum h (wac 7 hax))
           ==
    ==
  =+  l=:(mix (ror 0 28 a) (ror 0 34 a) (ror 0 39 a))   ::  S0
  =+  m=:(mix (dis a b) (dis a c) (dis b c))            ::  maj
  =+  n=(sum l m)                                       ::  t2
  =+  o=:(mix (ror 0 14 e) (ror 0 18 e) (ror 0 41 e))   ::  S1
  =+  p=(mix (dis e f) (dis (inv e) g))                 ::  ch
  =+  q=:(sum h o p (wac j kbx) (wac j wox))            ::  t1
  $(j +(j), a (sum q n), b a, c b, d c, e (sum d q), f e, g f, h g)
::  ed25519
++  ed
  =>
    =+  =+  [b=256 q=(sub (bex 255) 19)]
        =+  fq=~(. fo q)
        =+  ^=  l
             %+  add
               (bex 252)
             27.742.317.777.372.353.535.851.937.790.883.648.493
        =+  d=(dif.fq 0 (fra.fq 121.665 121.666))
        =+  ii=(exp.fq (div (dec q) 4) 2)
        [b=b q=q fq=fq l=l d=d ii=ii]
    |%
    ::                                                ::  ++norm:ed:crypto
    ++  norm                                          ::
      |=(x=@ ?:(=(0 (mod x 2)) x (sub q x)))
    ::                                                ::  ++xrec:ed:crypto
    ++  xrec                                          ::  recover x-coord
      |=  y=@  ^-  @
      =+  ^=  xx
          %+  mul  (dif.fq (mul y y) 1)
                   (inv.fq +(:(mul d y y)))
      =+  x=(exp.fq (div (add 3 q) 8) xx)
      ?:  !=(0 (dif.fq (mul x x) (sit.fq xx)))
        (norm (pro.fq x ii))
      (norm x)
    ::                                                ::  ++ward:ed:crypto
    ++  ward                                          ::  edwards multiply
      |=  [pp=[@ @] qq=[@ @]]  ^-  [@ @]
      =+  dp=:(pro.fq d -.pp -.qq +.pp +.qq)
      =+  ^=  xt
          %+  pro.fq
            %+  sum.fq
              (pro.fq -.pp +.qq)
            (pro.fq -.qq +.pp)
          (inv.fq (sum.fq 1 dp))
      =+  ^=  yt
          %+  pro.fq
            %+  sum.fq
              (pro.fq +.pp +.qq)
            (pro.fq -.pp -.qq)
          (inv.fq (dif.fq 1 dp))
      [xt yt]
    ::                                                ::  ++scam:ed:crypto
    ++  scam                                          ::  scalar multiply
      |=  [pp=[@ @] e=@]  ^-  [@ @]
      ?:  =(0 e)
        [0 1]
      =+  qq=$(e (div e 2))
      =>  .(qq (ward qq qq))
      ?:  =(1 (dis 1 e))
        (ward qq pp)
      qq
    ::                                                ::  ++etch:ed:crypto
    ++  etch                                          ::  encode point
      |=  pp=[@ @]  ^-  @
      (can 0 ~[[(sub b 1) +.pp] [1 (dis 1 -.pp)]])
    ::                                                ::  ++curv:ed:crypto
    ++  curv                                          ::  point on curve?
      |=  [x=@ y=@]  ^-  ?
      .=  0
          %+  dif.fq
            %+  sum.fq
              (pro.fq (sub q (sit.fq x)) x)
            (pro.fq y y)
          (sum.fq 1 :(pro.fq d x x y y))
    ::                                                ::  ++deco:ed:crypto
    ++  deco                                          ::  decode point
      |=  s=@  ^-  (unit [@ @])
      =+  y=(cut 0 [0 (dec b)] s)
      =+  si=(cut 0 [(dec b) 1] s)
      =+  x=(xrec y)
      =>  .(x ?:(!=(si (dis 1 x)) (sub q x) x))
      =+  pp=[x y]
      ?.  (curv pp)
        ~
      [~ pp]
    ::                                                ::  ++bb:ed:crypto
    ++  bb                                            ::
      =+  bby=(pro.fq 4 (inv.fq 5))
      [(xrec bby) bby]
    --  ::
  ~%  %ed  +  ~
  |%
  ::
  ++  point-add
    ~/  %point-add
    |=  [a-point=@udpoint b-point=@udpoint]
    ^-  @udpoint
    ::
    =/  a-point-decoded=[@ @]  (need (deco a-point))
    =/  b-point-decoded=[@ @]  (need (deco b-point))
    ::
    %-  etch
    (ward a-point-decoded b-point-decoded)
  ::
  ++  scalarmult
    ~/  %scalarmult
    |=  [a=@udscalar a-point=@udpoint]
    ^-  @udpoint
    ::
    =/  a-point-decoded=[@ @]  (need (deco a-point))
    ::
    %-  etch
    (scam a-point-decoded a)
  ::
  ++  scalarmult-base
    ~/  %scalarmult-base
    |=  scalar=@udscalar
    ^-  @udpoint
    %-  etch
    (scam bb scalar)
  ::
  ++  add-scalarmult-scalarmult-base
    ~/  %add-scalarmult-scalarmult-base
    |=  [a=@udscalar a-point=@udpoint b=@udscalar]
    ^-  @udpoint
    ::
    =/  a-point-decoded=[@ @]  (need (deco a-point))
    ::
    %-  etch
    %+  ward
      (scam bb b)
    (scam a-point-decoded a)
  ::
  ++  add-double-scalarmult
    ~/  %add-double-scalarmult
    |=  [a=@udscalar a-point=@udpoint b=@udscalar b-point=@udpoint]
    ^-  @udpoint
    ::
    =/  a-point-decoded=[@ @]  (need (deco a-point))
    =/  b-point-decoded=[@ @]  (need (deco b-point))
    ::
    %-  etch
    %+  ward
      (scam a-point-decoded a)
    (scam b-point-decoded b)
  ::                                                  ::  ++puck:ed:crypto
  ++  puck                                            ::  public key
    ~/  %puck
    |=  sk=@I  ^-  @
    ?:  (gth (met 3 sk) 32)  !!
    =+  h=(shal (rsh [0 3] b) sk)
    =+  ^=  a
        %+  add
          (bex (sub b 2))
        (lsh [0 3] (cut 0 [3 (sub b 5)] h))
    =+  aa=(scam bb a)
    (etch aa)
  ::                                                  ::  ++suck:ed:crypto
  ++  suck                                            ::  keypair from seed
    |=  se=@I  ^-  @uJ
    =+  pu=(puck se)
    (can 0 ~[[b se] [b pu]])
  ::                                                  ::  ++shar:ed:crypto
  ++  shar                                            ::  curve25519 secret
    ~/  %shar
    |=  [pub=@ sek=@]
    ^-  @ux
    =+  exp=(shal (rsh [0 3] b) (suck sek))
    =.  exp  (dis exp (can 0 ~[[3 0] [251 (fil 0 251 1)]]))
    =.  exp  (con exp (lsh [3 31] 0b100.0000))
    =+  prv=(end 8 exp)
    =+  crv=(fra.fq (sum.fq 1 pub) (dif.fq 1 pub))
    (curt prv crv)
  ::                                                  ::  ++sign:ed:crypto
  ++  sign                                            ::  certify
    ~/  %sign
    |=  [m=@ se=@]  ^-  @
    =+  sk=(suck se)
    =+  pk=(cut 0 [b b] sk)
    =+  h=(shal (rsh [0 3] b) sk)
    =+  ^=  a
        %+  add
          (bex (sub b 2))
        (lsh [0 3] (cut 0 [3 (sub b 5)] h))
    =+  ^=  r
        =+  hm=(cut 0 [b b] h)
        =+  ^=  i
            %+  can  0
            :~  [b hm]
                [(met 0 m) m]
            ==
        (shaz i)
    =+  rr=(scam bb r)
    =+  ^=  ss
        =+  er=(etch rr)
        =+  ^=  ha
            %+  can  0
            :~  [b er]
                [b pk]
                [(met 0 m) m]
            ==
        (~(sit fo l) (add r (mul (shaz ha) a)))
    (can 0 ~[[b (etch rr)] [b ss]])
  ::                                                  ::  ++veri:ed:crypto
  ++  veri                                            ::  validate
    ~/  %veri
    |=  [s=@ m=@ pk=@]  ^-  ?
    ?:  (gth (div b 4) (met 3 s))  |
    ?:  (gth (div b 8) (met 3 pk))  |
    =+  cb=(rsh [0 3] b)
    =+  rr=(deco (cut 0 [0 b] s))
    ?~  rr  |
    =+  aa=(deco pk)
    ?~  aa  |
    =+  ss=(cut 0 [b b] s)
    =+  ha=(can 3 ~[[cb (etch u.rr)] [cb pk] [(met 3 m) m]])
    =+  h=(shaz ha)
    =((scam bb ss) (ward u.rr (scam u.aa h)))
  --  ::ed
--
==
