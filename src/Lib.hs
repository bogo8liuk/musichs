{-# LANGUAGE TupleSections #-}

module Lib
    ( module Euterpea
    , defaultOctave
    , start
    , basePitch
    , ftrans
    , hNote
    , hNotes
    , hNotes'
    , graceNote
    , par
    , addDur
    , addDurPar
    , playMusic
    , quietLineToList
) where

import Data.List(foldl')
import Euterpea

defaultOctave :: Octave
defaultOctave = 4

start :: Music Pitch
start = rest 0

basePitch :: Pitch
basePitch = (C, 1)

ftrans :: Int -> Music Pitch -> Music Pitch
ftrans = fmap . trans

-- Harmonize Note
hNote :: Dur -> Pitch -> Int -> Music Pitch
hNote d p semitones = note d p :=: note d (trans semitones p)

hNotes :: Dur -> [Pitch] -> Int -> Music Pitch
hNotes dur ps semitones = hNotes' $ map (dur, , semitones) ps

hNotes' :: [(Dur, Pitch, Int)] -> Music Pitch
hNotes' = foldl' (\n (d, p, semitones) -> n :+: hNote d p semitones) (rest 0)

graceNote :: Int -> Music Pitch -> Music Pitch
graceNote n (Prim (Note dur pitch)) =
    note (dur / 8) (trans n pitch) :+: note (7 * dur /8) pitch
graceNote _ m = m

par :: Foldable t => t (Music a) -> Music a
par = foldl' (:=:) (rest 0)

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur dur takes = line $ map (\t -> t dur) takes

addDurPar :: Dur -> [Dur -> Music a] -> Music a
addDurPar dur takes = par $ map (\t -> t dur) takes

{-
Same of @lineToList@, but not throwing an error. NB: it doesn't visit in depth
the @Music@ structure.
-}
quietLineToList :: Music a -> [Music a]
quietLineToList (n :+: ns) = n : quietLineToList ns
quietLineToList m = [m]

playMusic :: Music Pitch -> IO ()
playMusic = playDev 2
