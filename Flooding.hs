
module Flooding where

import Core
import Control.Monad.State

defaultWater      = 0
defaultFlooding   = 0
defaultWaterproof = 10

isUnderwater :: Pos -> Int -> Bool
isUnderwater (Pos (x, y)) n = n >= y 

floodMine :: Mine -> (Int, Int, Int) -> Mine
floodMine mn = undefined
