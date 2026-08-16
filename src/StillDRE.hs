module StillDRE
  ( stillDRE
) where

import Lib

stillDRE :: Music Pitch
stillDRE = cut 16 $ tempo (93/120) (parFrom 3 melody drums)
  where
    drums = rest beginDur :+: chord [drums1, drums2] --, drums3]

    drums1 = line
      [ times 5 enr
      , times 3 (perc ClosedHiHat en)
      , times 16 $ roll en $ perc ClosedHiHat 2
      ]

    drums2 = line
      [ times 6 enr
      , drums2Hit en
      , enr
      , times 16 drums2Loop
      ]

    drums3 = line
      [ wnr
      , times 16 drums3Loop
      ]

    drums2Loop = line [enr, enr, drums2Hit sn, snr, enr, enr, enr, drums2Hit en,
      enr]

    drums2Hit = perc ElectricSnare

    drums3Loop = line [drums3Hit en, enr, snr, drums3Hit en, snr, drums3Hit en,
      enr, enr, enr]

    drums3Hit = perc LowFloorTom

    melody = cut 200 (mainMel :=: bassClefMel)

    mainMel = rest beginDur :+: repeatM (line [times 8 note1, times 3 note2, times 5 note3])

    note1 = chord [a 5 en, e 5 en, c 5 en]

    note2 = chord [a 5 en, e 5 en, b 4 en]

    note3 = chord [g 5 en, e 5 en, b 4 en]

    bassClefMel = e 2 beginDur :+: repeatM bassLoop

    bassLoop = line [a 2 qn, qnr, qnr, b 2 qn, e 2 qn, qnr, qnr, e 2 qn]

    beginDur = qn
