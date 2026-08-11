module LoseYourself
  ( loseYourself
) where

import Lib

loseYourself :: Music Pitch
loseYourself =
  mainMel :+:
  rest 0
  where
    shots = instrument Gunshot (a 4 sn :+: a 4 sn :+: a 4 sn :+: rest sn :+: a 4 qn)

    mainMel = mainIntroMel :+: beat :+: beat

    mainIntroMel = line [introMel3, introMel6, introMel3, introMel5, introMel9]

    introMel9 = introMel7 :=: introMel8

    introMel8 = line [g 3 qn, bf 3 qn, d 4 qn, bf 3 qn, f 3 qn, a 3 qn, c 4 qn,
      a 3 qn, introBassLongNote, a 2 qn, cs 3 qn, e 3 qn, a 3 qn, cs 4 qn,
      rest (qn + hn), rest bn]

    introBassLongNote = e 3 bn :=: e 2 bn

    introMel7 = line [d 5 qn, bf 4 qn, g 4 qn, d 5 qn, c 5 hn, rest qn, bf 4 en,
      a 4 en, g 4 wn, rest hn, a 4 qn, bf 4 qn, cs 5 wn, rest qn, e 4 qn,
      a 4 qn, b 4 qn, cs 5 wn, rest wn]

    introMel6 = introMel4 :=: introMel5

    introMel5 = line [bf 3 qn, d 4 qn, f 4 hn]

    introMel4 = line [rest qn, rest qn, c 6 en, bf 5 en, a 5 en, g 5 en]

    introMel3 = introMel1 :=: introMel2

    introMel2 = line [d 4 qn, f 4 qn, a 4 qn, f 4 qn, c 4 qn, e 4 qn,
      g 4 qn, e 4 qn, introMel5]

    introMel1 = line [a 5 qn, f 5 qn, d 5 qn, a 5 qn, g 5 qn, e 5 qn,
      c 5 qn, g 5 qn, a 5 qn, f 5 qn, d 5 hn]

    --bars 14-18
    beat = tempo (160/120) $ instrument AcousticGrandPiano
      (beatQuart1 :+: beatQuart1 :+: beatQuart2 :+: beatQuart3)

    beatQuart1 = times 4 beatNote1

    beatQuart2 = times 4 beatNote2

    beatQuart3 = times 3 beatNote2 :+: beatNote3 :+: beatNote4

    beatNote1 = d 3 qn :=: a 2 qn :=: d 2 qn

    beatNote2 = d 3 qn :=: bf 2 qn :=: d 2 qn

    beatNote3 = d 3 en :=: bf 2 en :=: d 2 en

    beatNote4 = d 3 en :=: g 2 en :=: d 2 en
