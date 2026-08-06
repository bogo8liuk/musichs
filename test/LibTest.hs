module LibTest
    ( testProperRow
) where

import Lib
import Test.HUnit (Test(..), assertBool, runTestTT, Counts)

testProperRow :: IO Counts
testProperRow = runTestTT $ TestList
    [ TestCase (assertBool "should be 12 tone row" $ properRow proper12ToneRow1)
    , TestCase (assertBool "should be 12 tone row" $ properRow proper12ToneRow2)
    , TestCase (assertBool "should be 12 tone row" $ properRow proper12ToneRow3)
    , TestCase (assertBool "parallel music is not 12 tone row" . not $
        properRow withPar1)
    , TestCase (assertBool
        "parallel music is not 12 tone row (even with rests only)" . not $
        properRow withPar2)
    , TestCase (assertBool "all notes in a 12 tone row are different" . not $
        properRow notAllDifferent)
    , TestCase (assertBool
        "number of notes in a twelve tone row must be exactly 12" . not $
        properRow notTwelve)
    ]
    where
        proper12ToneRow1 = a 5 qn :+: af 4 qn :+: ef 3 qn :+: e 3 qn :+: b 3 qn
            :+: ass 5 en :+: rest 2 :+: g 4 bn :+: gss 5 wn :+: gf 4 qn :+:
            c 8 bn :+: rest 3 :+: css 5 (qn / 3) :+: eff 8 bn

        proper12ToneRow2 = as 5 qn :+: af 4 qn :+: ef 3 qn :+: e 3 en :+: b 3 qn
            :+: ass 5 en :+: rest 0 :+: g 4 bn :+: gss 1 (wn / 2) :+: gf 4 qn
            :+: c 8 bn :+: rest 3 :+: css 5 (qn / 3) :+: dff 8 bn :+: rest 0

        proper12ToneRow3 = as 2 qn :+: af 2 qn :+: ef 7 qn :+: a 3 en :+: b 7 qn
            :+: bss 5 en :+: g 4 bn :+: gss 1 (wn / 2) :+: gf 4 qn :+: c 8 bn
            :+: css 5 (qn / 3) :+: fff 8 bn

        withPar1 = as 2 qn :+: af 2 qn :+: ef 7 qn :+: a 3 en :+: b 7 qn
            :+: bss 5 en :+: g 4 bn :+: gss 1 (wn / 2) :+: gf 4 qn :+: c 8 bn
            :+: css 5 (qn / 3) :+: (fff 8 bn :=: rest 0)

        withPar2 = as 2 qn :+: af 2 qn :+: ef 7 qn :+: a 3 en :+: b 7 qn
            :+: bss 5 en :+: g 4 bn :+: gss 1 (wn / 2) :+: gf 4 qn :+: c 8 bn
            :+: css 5 (qn / 3) :+: (rest 6 :=: rest 6) :+: fff 3 hn

        notAllDifferent = as 2 qn :+: as 3 bn :+: ef 7 qn :+: a 3 en :+: b 7 qn
            :+: bss 5 en :+: g 4 bn :+: gss 1 (wn / 2) :+: gf 4 qn :+: c 8 bn
            :+: css 5 (qn / 3) :+: fff 8 bn

        notTwelve = as 2 qn :+: af 2 qn :+: ef 7 qn :+: a 3 en :+: b 7 qn
            :+: bss 5 en :+: g 4 bn :+: gss 1 (wn / 2) :+: gf 4 qn :+: c 8 bn
            :+: css 5 (qn / 3)

properRow :: Music a -> Bool
properRow = undefined

