module Mozart
  ( pianoConcertNo24K491
) where

import Lib

pianoConcertNo24K491 :: Music Pitch
pianoConcertNo24K491 = bassoonMel
  where
    --b on c,f,g
    bassoonMel = instrument Bassoon $ line [m1, m2, m3, m4]

    m1 = addDur sn [cf 5, gf 5, e 5, cf 5]
    m2 = addDur sn [a 4, e 5, cf 5, a 4]
    m3 = addDur sn [ff 4, cf 5, a 4, ff 4]
    m4 = addDur sn [gf 4, d 5, b 4, gf 4]
