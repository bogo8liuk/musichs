module BinksRum
    ( binksRum
) where

import Lib

rr1 :: Music Pitch
rr1 = addDur (qn / 3) [g 4, bf 4, bf 4] :+: bf 4 hn

rr2 :: Music Pitch
rr2 = addDur (qn / 2) [c 5, bf 4] :+: g 4 qn

re1 :: Music Pitch
re1 = bf 4 (hn + qn)

re2 :: Music Pitch
re2 = ef 4 qn

r1 :: Music Pitch
r1 = rr1 :+: rr2 :+: re1

r2 :: Music Pitch
r2 = rr1 :+: rr2 :+: re2

yohoho :: Music Pitch
yohoho = r1 :+: r2

kij :: Music Pitch
kij = addDur en [bf 5, g 5, f 5, ef 5, c 5]

kij1 :: Music Pitch
kij1 = d 5 en :+: ef 5 qn

kij2 :: Music Pitch
kij2 = ef 5 en :+: bf 4 qn

kiij :: Music Pitch
kiij = addDur en [c 5, ef 5] :+: bf 4 qn :+:
    addDur en [c 5, d 5, ef 5, gs 5]

ki :: Music Pitch
ki = kij :+: kij1 :+: kij :+: kij2 :+: kiij

ke1 :: Music Pitch
ke1 = addDur en [g 5, g 5, f 5, ef 5] :+: f 5 hn

ke2 :: Music Pitch
ke2 = addDur en [g 5, ef 5, f 5, f 5] :+: ef 5 hn

k :: Music Pitch
k = ki :+: ke1 :+: ki :+: ke2

binksRum :: Music Pitch
binksRum = instrument AcousticGrandPiano (k :+: yohoho :+: yohoho)
