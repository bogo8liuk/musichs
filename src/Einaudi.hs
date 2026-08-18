module Einaudi
  ( experience
  , violin1Experience
  , violin2Experience
  , violaExperience
) where

import Lib

experience :: Music Pitch
experience = tempo experienceTempo $ chord [violin1Experience,
  violin2Experience, violaExperience]--, vln2, vla, cello]
  where
    cello = undefined

--s in g,f,c
violin1Experience :: Music Pitch
violin1Experience = tempo experienceTempo $ instrument Violin $ line [q1, q2,
  q3, q4, q5, q6, q7, q8]
  where
    q1 = phrase [Dyn $ StdLoudness PP] $ times 4 m1
    q2 = q1
    q3 = phrase [Dyn $ StdLoudness PP] $ times 4 m2
    q4 = phrase [Dyn $ StdLoudness PP] $ line [m1, m3, m1, m4]
    q5 = phrase [Dyn $ StdLoudness P] $ times 4 m5
    q6 = q5
    q7 = phrase [Dyn $ StdLoudness P] $ times 4 m6
    q8 = q5

    m1 = addDur sn [cs 5, fs 4, a 4, fs 4]
    m2 = addDur sn [cs 5, fs 4, gs 4, fs 4]
    m3 = addDur sn [b 4, fs 4, a 4, fs 4]
    m4 = addDur sn [d 5, fs 4, a 4, fs 4]
    m5 = addDur sn [a 4, fs 4, cs 5, fs 4]
    m6 = addDur sn [gs 4, fs 4, cs 5, fs 4]

--s in g,f,c
violin2Experience :: Music Pitch
violin2Experience = tempo experienceTempo $ instrument Violin $ line [q1, q2,
  q3, q4, q5, q6, q7, q8]
  where
    q1 = phrase [Dyn $ StdLoudness PP] $ times 4 m1
    q2 = q1
    q3 = phrase [Dyn $ StdLoudness PP] $ times 4 m2
    q4 = phrase [Dyn $ StdLoudness PP] m3
    q5 = phrase [Dyn $ StdLoudness P] $ times 4 m4
    q6 = q5
    q7 = phrase [Dyn $ StdLoudness P] $ times 4 m5
    q8 = q5

    m1 = cs 5 en :+: a 4 en
    m2 = cs 5 en :+: gs 4 en
    m3 = addDur en [cs 5, a 4, b 4, a 4, cs 5, a 4, d 5, a 4]
    m4 = a 4 en :+: cs 5 en
    m5 = line [gs 4 sn, fs 4 sn, cs 5 en]

--s on g,f,c
violaExperience :: Music Pitch
violaExperience = tempo experienceTempo $ instrument Viola $ line [q1, q2, q3,
  q4, q5, q6, q7, q8]
  where
    q1 = phrase [Dyn $ StdLoudness PP] $ times 4 m1
    q2 = q1
    q3 = q1
    q4 = phrase [Dyn $ StdLoudness PP] m2
    q5 = phrase [Dyn $ StdLoudness PP] $ times 4 m3
    q6 = q5
    q7 = phrase [Dyn $ StdLoudness PP] m4
    q8 = q5

    m1 = cs 5 qn
    m2 = line [cs 5 qn, b 4 qn, cs 5 qn, d 5 qn]
    m3 = a 4 qn
    m4 = gs 4 wn

--s on g,f,c
celloExperience :: Music Pitch
celloExperience = tempo experienceTempo $ instrument Cello $ undefined
  where
    q1 = phrase [Dyn $ StdLoudness PP] m1

    m1 = addDurPar wn [fs 3, fs 2]

experienceTempo :: Dur
experienceTempo = 92/120
