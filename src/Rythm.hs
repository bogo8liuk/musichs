module Rythm
  ( basicRythm
  , pr1
  , pr2
) where

import Euterpea
import Lib

basicRythm :: Int -> Music Pitch
basicRythm = repeatN metronome
  where
    metronome = metro :+: metro :+: metro :+: metro

    metro = instrument Percussion $ a 4 den

pr1, pr2 :: Pitch -> Music Pitch
pr1 p = tempo (5/6)
  (tempo (4/3)
    (mkLn 1 p qn :+:
    tempo (3/2) (
      mkLn 3 p en :+:
      mkLn 2 p sn :+:
      mkLn 1 p qn
    ) :+:
    mkLn 1 p qn
    ) :+:
  tempo (3/2) (mkLn 6 p en)
  )
pr2 p =
  let m1 = tempo (5/4) (tempo (3/2) m2 :+: m2 )
      m2 = mkLn 3 p en
  in tempo (7/6) (m1 :+: tempo (5/4) (mkLn 5 p en) :+: m1 :+: tempo (3/2) m2)

mkLn :: Int -> a -> Dur -> Music a
mkLn n p d = line $ take n $ repeat $ note d p
