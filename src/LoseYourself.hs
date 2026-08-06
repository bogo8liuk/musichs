module LoseYourself
  ( loseYourself
) where

import Lib

loseYourself :: Music Pitch
loseYourself =
  shots :+:
  instrument ChurchOrgan mel1 :+:
  (instrument MusicBox mel1 :=: instrument PizzicatoStrings mel2) :+:
  rest 0
  where
    shots = instrument Gunshot (a 4 sn :+: a 4 sn :+: a 4 sn :+: rest sn :+: a 4 qn)

    mel1 = a 5 qn :+: f 5 qn :+: d 5 qn :+: a 5 qn :+: g 5 qn :+: e 5 qn :+:
      c 5 qn :+: g 5 qn :+: a 5 qn :+: f 5 qn :+: d 5 hn

    mel2 = d 4 qn :+: f 4 qn :+: a 4 qn :+: f 4 qn :+: c 4 qn :+: e 4 qn :+:
      g 4 qn :+: e 4 qn :+: b 3 qn :+: d 4 qn :+: f 4 hn
