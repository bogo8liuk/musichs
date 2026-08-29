module Rythm
  ( basicDrums
  , accompanyingSnare
  , trapDrums
  , boomBapDrums
  , accompanyingSnare1
  , boomBapDrums1
  , accompanyingSnare2
  , boomBapDrums2
  , trapDrums1
  , trapDrums2
  , trapDrums3
  , trapDrums4
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

trapDrums1 :: Music Pitch
trapDrums1 = repeatM (drums1 :+: drums2)
  where
    drums1 = line [times 3 hit :=: snare1, snr, hit, snr,
      times 5 hit :=: (enr :+: snare2), snr, times 2 hit, times 4 hhit]

    drums2 = line [times 3 hit, snr, hit :=: snare1, snr,
      times 5 hit :=: (enr :+: snare2), snr, times 2 hit, times 4 hhit]

    hit = perc percussion sn
    hhit = perc percussion tn

    snare1 = perc LowTom sn
    snare2 = perc ElectricSnare sn

    percussion = ClosedHiHat

trapDrums2 :: Music Pitch
trapDrums2 = repeatM drums1
  where
    drums1 = line [hit, snr, times 3 hit, snr,
      times 6 hit, snr, hit, times 4 hhit]

    hit = perc percussion sn
    hhit = perc percussion tn

    percussion = ClosedHiHat

trapDrums3 :: Music Pitch
trapDrums3 = repeatM drums1
  where
    drums1 = line [hit :=: snare1, snr, times 3 hit, snr,
      times 6 hit :=: (snare2 :+: snr :+: snare1), snr, hit, times 4 hhit]

    hit = perc percussion sn
    hhit = perc percussion tn

    snare1 = perc LowTom sn
    snare2 = perc ElectricSnare sn

    percussion = ClosedHiHat

trapDrums4 :: Music Pitch
trapDrums4 = repeatM (drums1 :+: drums2)
  where
    drums1 = line [hit en, times 3 enr, snare en, times 3 enr]
    drums2 = line [times 8 $ hit tn, {-1/4-}enr, hit en, {-2/4-}snare en,
      times 3 enr]

    hit = perc percussion

    percussion = ClosedHiHat
    snare = perc LowTom
