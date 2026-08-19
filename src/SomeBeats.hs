module SomeBeats
  ( beat1
  , beat2
  , synthWinter
  , beat3_70bpm
  , beat3_92bpm
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
synthWinter = cut 16 $ drums :=: melody
  where
    melody = phrase [Dyn $ StdLoudness P] $ instrument SynthBrass2 (repeatM winter)

    drums = phrase [Dyn $ StdLoudness FFF] $ tempo (146/120) trapDrums

beat3_92bpm :: Music Pitch
beat3_92bpm = tempo (92/120) $ cut 16 beat3Sample

beat3_70bpm :: Music Pitch
beat3_70bpm = tempo (70/120) $ cut 16 beat3Sample

beat3Sample :: Music Pitch
beat3Sample = repeatM (repeatM (times 3 m :=: h) :=: boomBapDrums)
  where
    m = instrument AcousticGrandPiano $ phrase [Dyn $ StdLoudness MF] $
      line [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16]

    m1 = addDurPar sn [e 3, g 3, d 4]
    m2 = addDurPar sn [e 3, gs 3, d 4]
    m3 = m1
    m4 = transpose 3 m1
    m5 = addDurPar sn [cs 3, e 3, a 3]
    m6 = addDurPar sn [cs 3, es 3, a 3]
    m7 = m5
    m8 = transpose 3 m5
    m9 = m1
    m10 = m2
    m11 = m3
    m12 = m4
    m13 = addDurPar sn [af 3, c 3, f 4]
    m14 = addDurPar sn [af 3, cs 3, f 4]
    m15 = m13
    m16 = transpose 3 m13

    h = instrument OrchestraHit $ phrase [Dyn $ StdLoudness FFF] $ line [rest (bn + wn), b 3 en,
      hnr, f 3 en, enr, g 3 en]
