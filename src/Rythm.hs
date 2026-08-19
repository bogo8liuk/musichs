module Rythm
  ( basicDrums
  , accompanyingSnare
  , trapDrums
  , boomBapDrums
  , accompanyingSnare1
  , boomBapDrums1
  , accompanyingSnare2
  , boomBapDrums2
) where

import Euterpea
import Lib

basicDrums :: Music Pitch
basicDrums = repeatM $ roll en $ perc ClosedHiHat 2

accompanyingSnare :: Music Pitch
accompanyingSnare = repeatM drums
  where
    drums = line [enr, enr, drumsHit sn, snr, enr, enr, enr, drumsHit en, enr]

    drumsHit = perc ElectricSnare

boomBapDrums :: Music Pitch
boomBapDrums = basicDrums :=: accompanyingSnare

trapDrums :: Music Pitch
trapDrums = repeatM drums
  where
    drums = line [times 3 hit, snr, hit, snr, times 5 hit, snr, times 2 hit,
      times 4 hhit]

    hit = perc percussion sn

    hhit = perc percussion tn

    percussion = ClosedHiHat

boomBapDrums1 :: Music Pitch
boomBapDrums1 = basicDrums :=: accompanyingSnare

accompanyingSnare1 :: Music Pitch
accompanyingSnare1 = repeatM drums
  where
    drums = line [snareHit2 en, enr, snareHit1 sn, snareHit2 sn, enr,
      snareHit2 en, enr, snareHit1 en, enr]

    snareHit1 = perc ElectricSnare

    snareHit2 = perc HiMidTom

boomBapDrums2 :: Music Pitch
boomBapDrums2 = basicDrums :=: accompanyingSnare

accompanyingSnare2 :: Music Pitch
accompanyingSnare2 = repeatM drums
  where
    drums = line [enr, enr, snareHit1 sn, snareHit2 sn, enr,
      enr, enr, snareHit1 en, enr]

    snareHit1 = perc ElectricSnare

    snareHit2 = perc HiMidTom
