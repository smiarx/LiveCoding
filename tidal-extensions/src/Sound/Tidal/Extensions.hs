module Sound.Tidal.Extensions where

import 		Sound.Tidal.Context
import 		Data.List (groupBy)
import		Data.Maybe (mapMaybe)
import		Sound.Tidal.Utils (enumerate)

strum :: Pattern Time -> Pattern a -> Pattern a
strum tp p = innerJoin $ (\tv -> _strum tv p) <$> tp

_strum :: Time -> Pattern a -> Pattern a
_strum t p = withEvents munge p
  where munge es = concatMap spreadOut (groupBy (\a b -> whole a == whole b) es)
        spreadOut xs = mapMaybe (\(n, x) -> shiftIt n x) $ enumerate xs
        shiftIt n (Event (Arc s e) a' v) =
          do
            a'' <- subArc (Arc newS e) a'
            return (Event (Arc newS e) a'' v)
          where newS = s + (t * fromIntegral n)

-- PARAMETERS
sidechain, sdch :: Pattern Double -> ControlPattern
sidechain = pF "sidechain"
sdch = sidechain

phaserfeedback, phasfb :: Pattern Double -> ControlPattern
phaserfeedback = pF "phaserfeedback"
phasfb = phaserfeedback


-- replace cutoff by lpf, cutoff used for synths
lpf :: Pattern Double -> ControlPattern
lpf = pF "lpf"
