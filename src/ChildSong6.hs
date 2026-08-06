module ChildSong6
    ( childSong6
) where

import Lib

b1 :: Music Pitch
b1 = addDur dqn [b 3, fs 4, g 4, fs 4]

b2 :: Music Pitch
b2 = addDur dqn [b 3, es 4, fs 4, es 4]

b3 :: Music Pitch
b3 = addDur dqn [as 3, fs 4, g 4, fs 4]

bassLine :: Music Pitch
bassLine = times 3 b1 :+: times 2 b2 :+: times 4 b3 :+: times 5 b1


v1a :: Music Pitch
v1a = addDur en [a 5, e 5, d 5, fs 5, cs 5, b 4, e 5, b 4]

v1b :: Music Pitch
v1b = addDur en [cs 5, b 4]

v1 :: Music Pitch
v1 = v1a :+: graceNote (-1) (d 5 qn) :+: v1b

v2a :: Music Pitch
v2a = line [cs 5 (dhn + dhn), d 5 dhn, f 5 hn, gs 5 qn, fs 5 (hn + en), g 5 en ] -- bars 7-11

v2b :: Music Pitch
v2b = addDur en [fs 5, e 5, cs 5, as 4] :+: a 4 dqn :+: addDur en [as 4, cs 5, fs 5, e 5, fs 5] -- bars 12-13

v2c :: Music Pitch
v2c = line [g 5 en, as 5 en, cs 6 (hn + en), d 6 en, cs 6 en ] :+: e 5 en :+: enr :+: line [as 5 en, a 5 en, g 5 en, d 5 qn, c 5 en, cs 5 en ] -- bars 14-16

v2d :: Music Pitch
v2d = addDur en [fs 5, cs 5, e 5, cs 5, a 4, as 4, d 5, e 5, fs 5] -- bars 17-18.5

v2e :: Music Pitch
v2e = line [graceNote 2 (e 5 qn), d 5 en, graceNote 2 (d 5 qn), cs 5 en, graceNote 1 (cs 5 qn), b 4 (en + hn), cs 5 en, b 4 en ] -- bars 18.5-20

v2f :: Music Pitch
v2f = line [fs 5 en, a 5 en, b 5 (hn + qn), a 5 en, fs 5 en, e 5 qn, d 5 en, fs 5 en, e 5 hn, d 5 hn, fs 5 qn ] -- bars 21-23

v2g :: Music Pitch
v2g = tempo (3/2) (line [cs 5 en, d 5 en, cs 5 en ]) :+: b 4 (3 * dhn + hn) -- bars 24-28

v2 :: Music Pitch
v2 = v2a :+: v2b :+: v2c :+: v2d :+: v2e :+: v2f :+: v2g

mainVoice :: Music Pitch
mainVoice = times 3 v1 :+: v2

childSong6 :: Music Pitch
childSong6 =
    let t = (dhn / qn) * (69 / 120)
    in instrument RhodesPiano (tempo t (bassLine :=: mainVoice))

