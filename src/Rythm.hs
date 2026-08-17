module Rythm
  ( basicDrums
  , accompanyingDrums
  , trapDrums
) where

import Euterpea
import Lib

basicDrums :: Music Pitch
basicDrums = repeatM $ roll en $ perc ClosedHiHat 2

accompanyingDrums :: Music Pitch
accompanyingDrums = repeatM drums
  where
    drums = line [enr, enr, drumsHit sn, snr, enr, enr, enr, drumsHit en, enr]

    drumsHit = perc ElectricSnare

trapDrums :: Music Pitch
trapDrums = repeatM drums
  where
    drums = line [times 3 hit, snr, hit, snr, times 5 hit, snr, times 2 hit,
      times 4 hhit]

    hit = perc ClosedHiHat sn

    hhit = perc ClosedHiHat tn
