module Rythm
  ( basicRythm
) where

import Euterpea
import Lib

basicRythm :: Int -> Music Pitch
basicRythm = repeatN metronome
  where
    metronome = metro :+: metro :+: metro :+: metro

    metro = instrument Percussion $ a 4 den
