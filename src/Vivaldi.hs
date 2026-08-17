module Vivaldi
  ( winter
) where

import Lib

winter :: Music Pitch
winter = tempo (146/120) $ trebleClefMel :=: bassClefMel
  where
    --b on b,a,d,e
    trebleClefMel = line [trebleQ1, trebleQ2, trebleQ3, trebleQ4]

    --b on e,d,b,a
    bassClefMel = line [bassQ1, bassQ2, bassQ3, bassQ4]

    trebleQ1 = times 4 (n11 :+: n12)

    n11 = af 5 en :=: f 5 en

    n12 = c 5 en :=: af 4 en

    bassQ1 = times 4 (b11 :+: b12)

    b11 = f 2 en

    b12 = f 3 en

    trebleQ2 = line [n21, times 3 (nq2sn :+: n22), nq2en, times 3 n21]

    n21 = nq2en :=: f 4 en

    n22 = f 4 sn

    nq2base = [f 5, df 5, bf 4]

    nq2en = addDurPar en nq2base

    nq2sn = addDurPar sn nq2base

    bassQ2 = line [bf 2 en :=: bf 1 en, rest (wn - en)]

    trebleQ3 = times 4 (n31 :+: n32)

    n31 = g 5 en :=: ef 5 en

    n32 = bf 4 en :=: g 4 en

    bassQ3 = times 4 (b31 :+: b32)

    b31 = ef 2 en

    b32 = ef 3 en

    trebleQ4 = line [n41, times 3 (nq4sn :+: n42), nq4en, times 3 n41]

    n41 = nq4en :=: ef 4 en

    n42 = ef 4 sn

    nq4base = [ef 5, c 5, g 4]

    nq4en = addDurPar en nq4base

    nq4sn = addDurPar sn nq4base

    bassQ4 = line [af 2 en :=: af 1 en, rest (wn - en)]
