module SomeBeats
  ( beat1
  , beat2
  , synthWinter
  , winterBeat
) where

import Lib
import Rythm
import Vivaldi

beat1 :: Music Pitch
beat1 = cut 16 beat
  where
    beat = instrument HammondOrgan $ repeatM t251 :=: (basicDrums :=: accompanyingDrums)

beat2 :: Music Pitch
beat2 = cut 16 $ melody :=: beat1
  where
    melody = repeatM $ instrument Xylophone $ line [wnr, n1, n2, n3, n4]

    n1 = chord [f 5 en, d 5 en, g 4 en]

    n2 = chord [f 5 en, bf 5 en, g 4 en]

    n3 = chord [f 5 en, e 5 en, g 4 en]

    n4 = n1

t251 :: Music Pitch
t251 =
    let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
        gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
        cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
    in dMinor :+: gMajor :+: cMajor

synthWinter :: Music Pitch
synthWinter = cut 16 $ tempo (146/120) trapDrums :=: instrument SynthBrass2 (repeatM winter)
