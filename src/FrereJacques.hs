module FrereJacques
  ( frereJacques
) where

import Lib

frereJacques :: Music Pitch
frereJacques = chord [trebleClefMel, bassClefMel]
  where
    trebleClefMel = line [trebleClefMel1, trebleClefMel1, trebleClefMel2,
      trebleClefMel2, trebleClefMel3, trebleClefMel3, trebleClefMel4,
      trebleClefMel4]

    trebleClefMel1 = line [c 4 qn, d 4 qn, e 4 qn, c 4 qn]

    trebleClefMel2 = line [e 4 qn, f 4 qn, g 4 hn]

    trebleClefMel3 = line [g 4 den, a 4 sn, g 4 en, f 4 en, e 4 qn, c 4 qn]

    trebleClefMel4 = line [c 4 qn, g 4 qn, c 4 hn]

    bassClefMel = line [bassClefMel1, bassClefMel1, bassClefMel2, bassClefMel2,
      bassClefMel3, bassClefMel3, bassClefMel4, bassClefMel4]

    bassClefMel1 = line [c 3 hn, e 3 hn]

    bassClefMel2 = line [e 3 hn, g 3 hn]

    bassClefMel3 = line [g 3 hn, e 3 hn]

    bassClefMel4 = line [rest qn, g 3 hn, rest qn]
