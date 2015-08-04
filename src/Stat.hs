module Stat where

import IS
import Data.List
import Data.Function
import Control.Arrow

_passedBy :: ([ (Uco, Points) ] -> [[ (Uco, Points) ]] )
          -> [Uco] -> [ (Uco, Points) ] -> [ (Points, Int, Int, Double) ]
_passedBy sel succ = sel >>> filter (not . null) >>>
                     map (snd . head &&& (map fst >>> length &&& length . filter (`elem` succ))) >>>
                     map (\(pt, (all, succ)) -> (pt, all, succ, fromIntegral succ / fromIntegral all))

acc = tails >>> map concat

_passedByPts f = _passedBy (groupBy ((==) `on` snd) >>> f)

passedByPts = _passedByPts id
passedByPtsAcc = _passedByPts acc

_passedByPtsGr f n = _passedBy (groupBy ((==) `on` (snd >>> (/ n) >>> floor)) >>> f)

passedByPtsGr = _passedByPtsGr id
passedByPtsGrAcc = _passedByPtsGr acc
