
module Flooding (updateWater, robotDrowned, defaultFlooding, makeFloodingState) where

import Core
import Control.Monad.State

{- ***defined in Core***
data FloodingState = FloodingState
    { waterLevel         :: Int
    , floodingSpeed      :: Int
    , waterProofing      :: Int
    , stepsSinceLastRise :: Int
    , waterProofingLeft  :: Int } -}

makeFloodingState :: Int -> Int -> Int -> FloodingState
makeFloodingState level speed proofing = FloodingState level speed proofing 1 proofing

defaultFlooding = makeFloodingState 0 0 10

isUnderwater :: Mine -> Pos -> Bool
isUnderwater mine pos = isUnderwater' (flooding mine) pos

isUnderwater' flooding' (Pos _ y) = waterLevel flooding' >= y

stepWaterLevel :: FloodingState -> FloodingState
stepWaterLevel flooding
    | floodingSpeed flooding == 0 = flooding
    | rised                       = flooding{waterLevel = waterLevel flooding + 1, stepsSinceLastRise = 1}
    | otherwise                   = flooding{stepsSinceLastRise = stepsSinceLastRise flooding + 1}
    where rised = stepsSinceLastRise flooding >= floodingSpeed flooding

robotDrowned :: Mine -> Bool
robotDrowned mine = waterProofingLeft (flooding mine) < 0

stepWaterProofing :: FloodingState -> Pos -> FloodingState
stepWaterProofing flooding' pos
    | isUnderwater' flooding' pos = flooding'{waterProofingLeft = waterProofingLeft flooding' - 1}
    | otherwise                   = flooding'{waterProofingLeft = waterProofing flooding'}

updateWater :: Mine -> Mine
updateWater mine = mine{flooding = stepWaterProofing (stepWaterLevel (flooding mine)) (robotPos mine)}