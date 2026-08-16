module Rythm
  ( basicDrums
  , accompanyingDrums
) where

import Euterpea
import Lib

basicDrums :: Music Pitch
basicDrums = repeatM $ roll en $ perc ClosedHiHat 2

accompanyingDrums :: Music Pitch
accompanyingDrums = repeatM drums
  where
    drums =  line [enr, enr, drumsHit sn, snr, enr, enr, enr, drumsHit en, enr]

    drumsHit = perc ElectricSnare
