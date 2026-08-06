{-|
Module      : Scratch
Description : Music from scratch
Copyright   : (c) Luca Borghi, 2023
License     : GPL-3
Maintainer  : lucaborghi99@gmail.com
Stability   : experimental

A sound is nothing but a wave with a certain frequency. This wave is represented
as a string of bytes which will be stored in a file and read by ffmpeg to
effectively generate sound. The bytes are stored in little endian and they are
created with the "B.Builder" type, literally a builder to construct sequences of
bytes.
-}
module Scratch
    ( playScratch
) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as B
import System.Process (runCommand)
import Text.Printf (printf)

type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float

{- |
The volume is given by the height of the wave (arriving until to 1
and -1 is a volume very high).
-}
volume :: Float
volume = 0.5

duration :: Seconds
duration = 2.0

{- |
Ideally, a sound wave is continous, but a a continous wave cannot effectively
be stored. So we define a sample rate, namely how many floats (samples) we take
per second.
-}
sampleRate :: Samples
sampleRate = 48000.0

{- |
The frequency of the \"pitch standard\".
-}
pitchStandard :: Hz
pitchStandard = 440.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

{- |
To measure the \"tempo\" in a song (beats per minute).
-}
bpm :: Beats
bpm = 120.0

{- |
The semitone is a numbered note. This is the formula to calculate the frequency
of a semitone
-}
f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Semitones -> Seconds -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)

{-
In order to separate notes, we implement \"attack decay release\", namely the a
special flow for a note which is not completely flat.

^
|
|   /\
|  /  \____
| /        \
|/          \
+-------------->
 |__|_|___|__|
  1  2  3  4

1) attack
2) decay
3) sustain
4) release

This will give separation between notes.
-}

{- |
How many times per second something will happen? That something can be anything.
Another important thing to know is the following: a single cycle of the wave is
a single time the function goes up and goes down. How many times a wave cycle
happens per second depends by the function.
-}
freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
    map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output
    where
        {- |
        The step is used to spread the frequency (namely to make it lower) in
        order to be able to hear something 
        -}
        step = (hz * 2 * pi) / sampleRate

        output = map (sin . (* step)) [0.0 .. sampleRate * duration]

        attack :: [Pulse]
        attack = map (min 1.0) [0.0, 0.001 ..]

        release :: [Pulse]
        release = reverse $ take (length output) attack

{- |
A sound is a wave (the function sin is a wave).
-}
wave :: [Pulse]
wave =
    {- To create notes, it's necessary to vary the frequency. However, the
    difference of frequency between notes is not linear, but it follows a
    formula. -}
    concat
        [ note (-15) 1
        , note 0 0.25
        , note 0 0.25
        , note 0 0.25
        , note 0 0.25
        , note 0 0.5

        , note 0 0.25
        , note 0 0.25
        , note 0 0.25
        , note 0 0.25
        , note 0 0.5

        , note 3 0.25
        , note 3 0.25
        , note 3 0.25
        , note 3 0.25
        , note 3 0.5

        , note (-2) 0.25
        , note (-2) 0.25
        , note (-2) 0.25
        , note (-2) 0.25
        , note (-2) 0.5

        , note 0 0.25
        , note 0 0.25
        , note 0 0.25
        , note 0 0.25
        , note 0 0.5
        ]

bytesSequenceOf :: [Float] -> BS.ByteString
bytesSequenceOf = B.toLazyByteString . foldMap B.floatLE

filePath :: FilePath
filePath = "output.bin"

{- |
-}
save :: FilePath -> IO ()
save filePath = BS.writeFile filePath $ bytesSequenceOf wave

playScratch :: IO ()
playScratch = do
    save filePath
    --the -ar flag is the sample rate
    runCommand ffplay
    return ()
    where
        ffplay =
            printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate filePath
