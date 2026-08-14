module StillDRE
  ( stillDRE
) where

import Lib

stillDRE :: Music Pitch
stillDRE = cut 16 $ tempo (93/120) (parFrom 3 melody drums)
  where
    drums = line [
      rest beginDur,
      times 5 enr,
      times 3 (perc ClosedHiHat en),
      times 16 $ roll en $ perc ClosedHiHat 2
     ]

    melody = cut 200 (mainMel :=: bassClefMel)

    mainMel = rest beginDur :+: repeatM (line [times 8 note1, times 3 note2, times 5 note3])

    note1 = chord [a 5 en, e 5 en, c 5 en]

    note2 = chord [a 5 en, e 5 en, b 4 en]

    note3 = chord [g 5 en, e 5 en, b 4 en]

    bassClefMel = e 2 beginDur :+: repeatM bassLoop

    bassLoop = line [a 2 qn, qnr, qnr, b 2 qn, e 2 qn, qnr, qnr, e 2 qn]

    beginDur = qn
