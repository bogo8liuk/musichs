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
    , graceNote'
    , graceNote''
    , par
    , repeatN
    , repeatM
    , addDur
    , addDurPar
    , chromaticScale
    , chromaticScaleOf
    , scale
    , playMusic
    , quietLineToList
    , trill
    , trill'
    , trilln
    , trilln'
    , roll
    , rolln
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

--General version of graceNote
graceNote' :: Int -> Rational -> Music Pitch -> Music Pitch
graceNote' n r (Prim (Note dur pitch)) =
    note (r * dur) (trans n pitch) :+: note ((1 - r) * dur) pitch
graceNote' _ _ m = m

graceNote'' :: Int -> Rational -> Music Pitch -> Music Pitch -> Music Pitch
graceNote'' n r (Prim (Note dur pitch)) (Prim (Note dur' pitch')) =
    note (dur - r * dur') pitch :+: note (r * dur') (trans n pitch') :+:
    note dur' pitch'
graceNote'' _ _ m1 m2 = m1 :+: m2

par :: Foldable t => t (Music a) -> Music a
par = foldl' (:=:) (rest 0)

repeatN :: Music a -> Int -> Music a
repeatN m n
    | n <= 0 = rest 0
    | otherwise = m :+: repeatN m (n-1)

repeatM :: Music a -> Music a
repeatM m = m :+: repeatM m

chromaticScale :: Music Pitch
chromaticScale = line [c 4 qn, cs 4 qn, d 4 qn, ds 4 qn, e 4 qn, f 4 qn,
    fs 4 qn, g 4 qn, gs 4 qn, a 4 qn, as 4 qn, b 4 qn]

-- Sequential notes between p1 and p2, each distant one semitone and each with
-- a duration of 1/4
chromaticScaleOf :: Pitch -> Pitch -> Music Pitch
chromaticScaleOf p1 p2 =
    case compare absp1 absp2 of
        LT -> line (map (note qn . pitch) [absp1..absp2])
        EQ -> note qn p1
        GT -> line (map (note qn . pitch) $ reverse [absp2..absp1])
    where
        absp1 = absPitch p1

        absp2 = absPitch p2

-- Given a Pitch p and a list of intervals, it creates a chromatic scale with
-- the given intervals instead of taking one semitone.
scale :: Pitch -> [Int] -> Music Pitch
scale p intervals = line $ map (note qn . pitch) ps
    where
        ps = asc $ absp : intervals

        asc [] = []
        asc [x] = [x]
        asc (x1 : x2 : xs) = x1 : asc (x1+x2 : xs)

        absp = absPitch p

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

--Ornament that alternates rapidly between two pitches
--NB: it is defined only on single notes
trill :: Int -> Dur -> Music Pitch -> Music Pitch
trill i d (Prim (Note d' p)) =
    if d >= d'
    then note d' p
    else note d p :+: trill (-i) d (note (d' - d) (trans i p))
trill i d (Modify (Tempo t) m) = tempo t (trill i (d * t) m)
trill i d (Modify c m) = Modify c (trill i d m)
trill _ _ m = m

--Same of trill, but it starts from transposed note
trill' :: Int -> Dur -> Music Pitch -> Music Pitch
trill' i d m = trill (-i) d $ transpose i m

--Same of trill, but rather than specifying a duration of the trill, it
--specifies how many times it has to change note
trilln :: Int -> Int -> Music Pitch -> Music Pitch
trilln i n m = trill i (dur m / fromIntegral n) m

--Same of trilln, but it starts from transposed note
trilln' :: Int -> Int -> Music Pitch -> Music Pitch
trilln' i n m = trilln (-i) n $ transpose i m

--Special case of trill where note transposition is zero. Useful for percussion
roll :: Dur -> Music Pitch -> Music Pitch
roll = trill 0

rolln :: Int -> Music Pitch -> Music Pitch
rolln = trilln 0

playMusic :: Music Pitch -> IO ()
playMusic = playDev 2
