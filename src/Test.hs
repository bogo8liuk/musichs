module Test
    ( requiemIntro
    , randomMusic
    , tempoh
    , tryInvert
    , tryRetro
    , tryRetroInvert
    , tryInvertRetro
) where

import Data.List(foldl')
import Lib

t251 :: Music Pitch
t251 =
    let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
        gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
        cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
    in dMinor :+: gMajor :+: cMajor

simpleMountain :: InstrumentName -> Music Pitch
simpleMountain instr =
    instrument instr melody
    where
    melody = baseMelody -- :+: rest hn :+: reverseMelody
    baseMelody = foldl' (:+:) start basePitches
    reverseMelody = foldl' (:+:) start $ reverse basePitches

    basePitches =
        [fff actualOctave en, ff actualOctave en, f actualOctave en]

    actualOctave = 4

requiemIntro :: Music Pitch
requiemIntro = instrument PizzicatoStrings melody
    where
        melody = foldl' (:+:) start basePitches

        basePitches =
            [ d actualOctave dsn
            , ds actualOctave dsn
            , dss actualOctave dsn
            , ds actualOctave dsn
            , d actualOctave den
            , d actualOctave den
            ]

        actualOctave = 4

randomMusic :: Music Pitch
randomMusic = instrument Violin melody
    where
        melody = foldl' (:+:) start basePitches

        basePitches =
            [ basePitch
            , fmap (trans 1) basePitch
            , fmap (trans (-2)) basePitch
            ]

        basePitch =
            ff defaultOctave hn

tempoh :: Music Pitch
tempoh = melody
    where
        melody = foldl' (:+:) start [ baseMelody | _ <- [0..3] ]
        baseMelody = firstWave :+: secondWave :+: thirdWave

        firstWave = instrument ReverseCymbal $ foldl' (:+:) start [ shortPitch | _ <- [0..15] ]
        secondWave = instrument PizzicatoStrings longPitch
        thirdWave = instrument PizzicatoStrings $ foldl' (:+:) start [ basePitch | _ <- [0..1] ]

        shortPitch = basePartialNote tn
        basePitch = ftrans 2 $ basePartialNote en
        longPitch = basePartialNote qn

        basePartialNote = ff defaultOctave

tryInvert :: Music Pitch
tryInvert = simpleMountain AcousticGrandPiano :+: rest 1 :+: invert (simpleMountain AcousticGrandPiano)

tryRetro :: Music Pitch
tryRetro = simpleMountain AcousticGrandPiano :+: rest 1 :+: retro (simpleMountain AcousticGrandPiano)

tryRetroInvert :: Music Pitch
tryRetroInvert = simpleMountain AcousticGrandPiano :+: rest 1 :+: retroInvert (simpleMountain AcousticGrandPiano)

tryInvertRetro :: Music Pitch
tryInvertRetro = simpleMountain AcousticGrandPiano :+: rest 1 :+: invertRetro (simpleMountain AcousticGrandPiano)
