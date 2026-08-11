module FrereJacques
  ( frereJacques
  , frereJacquesRmx1
  , halfFrereJacques2
) where

import Lib

frereJacques :: Music Pitch
frereJacques = chord [trebleClefMel, bassClefMel]

frereJacquesRmx1 :: Music Pitch
frereJacquesRmx1 = line [frereJacques, tempo (160/120) frereJacques,
  tempo (200/120) frereJacques, times 2 (tempo (200/120) halfFrereJacques2)]

halfFrereJacques2 :: Music Pitch
halfFrereJacques2 = retro . cut (bn * 2) $ retro frereJacques

trebleClefMel :: Music Pitch
trebleClefMel = line [times 2 trebleClefMel1, times 2 trebleClefMel2,
  times 2 trebleClefMel3, times 2 trebleClefMel4]

trebleClefMel1 :: Music Pitch
trebleClefMel1 = line [c 4 qn, d 4 qn, e 4 qn, c 4 qn]

trebleClefMel2 :: Music Pitch
trebleClefMel2 = line [e 4 qn, f 4 qn, g 4 hn]

trebleClefMel3 :: Music Pitch
trebleClefMel3 = line [g 4 den, a 4 sn, g 4 en, f 4 en, e 4 qn, c 4 qn]

trebleClefMel4 :: Music Pitch
trebleClefMel4 = line [c 4 qn, g 4 qn, c 4 hn]

bassClefMel :: Music Pitch
bassClefMel = line [times 2 bassClefMel1, times 2 bassClefMel2,
  times 2 bassClefMel3, times 2 bassClefMel4]

bassClefMel1 :: Music Pitch
bassClefMel1 = line [c 3 hn, e 3 hn]

bassClefMel2 :: Music Pitch
bassClefMel2 = line [e 3 hn, g 3 hn]

bassClefMel3 :: Music Pitch
bassClefMel3 = line [g 3 hn, e 3 hn]

bassClefMel4 :: Music Pitch
bassClefMel4 = line [rest qn, g 3 hn, rest qn]
