
module Growths where

import Core

makeBeardGrowth :: Int -> Int -> BeardGrowth
makeBeardGrowth startRazors growRate = BeardGrowth startRazors growRate (growRate - 1)

-- will use surroundings and map all Beards to Empty PRIOR to moving: constitutes
-- a move
applyRazor :: Mine -> Mine
applyRazor mn = undefined