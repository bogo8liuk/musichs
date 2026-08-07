module LoseYourself
  ( loseYourself
) where

import Lib
import Rythm

loseYourself :: Music Pitch
loseYourself =
  shots :+:
  --(mainMel :=: basicRythm 8) :+:
  beat :+: beat :+:
  rest 0
  where
    mainMel = instrument ChurchOrgan mel1 :+:
      (instrument MusicBox mel1 :=: instrument PizzicatoStrings mel2)

    shots = instrument Gunshot (a 4 sn :+: a 4 sn :+: a 4 sn :+: rest sn :+: a 4 qn)

    mel1 = a 5 qn :+: f 5 qn :+: d 5 qn :+: a 5 qn :+: g 5 qn :+: e 5 qn :+:
      c 5 qn :+: g 5 qn :+: a 5 qn :+: f 5 qn :+: d 5 hn

    mel2 = d 4 qn :+: f 4 qn :+: a 4 qn :+: f 4 qn :+: c 4 qn :+: e 4 qn :+:
      g 4 qn :+: e 4 qn :+: b 3 qn :+: d 4 qn :+: f 4 hn

    beat = instrument AcousticGrandPiano
      (beatQuart1 :+: beatQuart1 :+: beatQuart2 :+: beatQuart3)

    beatQuart1 = repeatN beatNote1 4

    beatQuart2 = repeatN beatNote2 4

    beatQuart3 = repeatN beatNote2 3 :+: beatNote3 :+: beatNote4

    beatNote1 = d 3 qn :=: a 2 qn :=: d 2 qn

    beatNote2 = d 3 qn :=: b 2 qn :=: d 2 qn

    beatNote3 = d 3 en :=: b 2 en :=: d 2 en

    beatNote4 = d 3 en :=: g 2 en :=: d 2 en
