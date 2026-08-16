module Test
    ( requiemIntro
    , randomMusic
    , tempoh
    , tryInvert
    , tryRetro
    , tryRetroInvert
    , tryInvertRetro
    , pr1
    , pr2
    , funkGroove
    , t251
    , aGroove
    , aGroove1
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

pr1, pr2 :: Pitch -> Music Pitch
pr1 p = tempo (5/6)
  (tempo (4/3)
    (mkLn 1 p qn :+:
    tempo (3/2) (
      mkLn 3 p en :+:
      mkLn 2 p sn :+:
      mkLn 1 p qn
    ) :+:
    mkLn 1 p qn
    ) :+:
  tempo (3/2) (mkLn 6 p en)
  )
pr2 p =
  let m1 = tempo (5/4) (tempo (3/2) m2 :+: m2)
      m2 = mkLn 3 p en
  in tempo (7/6) (m1 :+: tempo (5/4) (mkLn 5 p en) :+: m1 :+: tempo (3/2) m2)

mkLn :: Int -> a -> Dur -> Music a
mkLn n p d = line $ take n $ repeat $ note d p

funkGroove :: Music Pitch
funkGroove =
  let p1 = perc LowTom qn
      p2 = perc AcousticSnare en
  in tempo 3 $ {-instrument Percussion $-} cut 8 $ repeatM
    ((p1 :+: qnr :+: p2 :+: qnr :+: p2 :+: p1 :+: p1 :+: qnr :+: p2 :+: enr) :=:
    roll en (perc ClosedHiHat 2))

aGroove :: Music Pitch
aGroove = rep (delayM qn) (transpose 1) 12 (es 2 qn)

aGroove1 :: Music Pitch
aGroove1 = rep (transpose 1) (transpose (-2)) 5 (es 4 qn)
