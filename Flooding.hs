
module Flooding where

import Core
import Control.Monad.State

{- ***defined in Core***
data FloodingState = FloodingState
    { waterLevel         :: Int
    , floodingSpeed      :: Int
    , waterProofing      :: Int
    , stepsSinceLastRise :: Int
    , waterProofingLeft  :: Int } -}

defaultWater      = 0
defaultFlooding   = 0
defaultWaterproof = 10

isUnderwater :: Mine -> Pos -> Bool
isUnderwater mine (Pos (x, y)) = waterLevel (flooding mine) >= y 

stepFloodingState :: FloodingState -> FloodingState
stepFloodingState flooding
    | floodingSpeed flooding == 0 = flooding
    | rised                       = flooding{waterLevel = waterLevel flooding + 1, stepsSinceLastRise = 1}
    | otherwise                   = flooding{stepsSinceLastRise = stepsSinceLastRise flooding + 1}
    where rised = stepsSinceLastRise flooding == floodingSpeed flooding
