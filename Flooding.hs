{-# LANGUAGE Rank2Types #-}
module Flooding where

import Core
import Control.Monad.State

makeFloodingState :: Int -> Int -> Int -> FloodingState
makeFloodingState level speed proofing = FloodingState level speed proofing 1 proofing

defaultFlooding = makeFloodingState 0 0 10

isUnderwater :: (forall s. MutableMine s) -> Pos -> Bool
isUnderwater (_,info) pos = isUnderwater' (flooding info) pos

isUnderwater' flooding' (Pos _ y) = waterLevel flooding' >= y

stepWaterLevel :: FloodingState -> FloodingState
stepWaterLevel flooding
    | floodingSpeed flooding == 0 = flooding
    | rised                       = flooding{waterLevel = waterLevel flooding + 1, stepsSinceLastRise = 1}
    | otherwise                   = flooding{stepsSinceLastRise = stepsSinceLastRise flooding + 1}
    where rised = stepsSinceLastRise flooding >= floodingSpeed flooding

-- return Nothing if the robot drowned
stepWaterProofing :: FloodingState -> Pos -> Maybe FloodingState
stepWaterProofing flooding' pos
    | isUnderwater'' && waterProofingLeft' <= 0 = Nothing
    | isUnderwater''                            = Just (flooding'{waterProofingLeft = waterProofingLeft' - 1})
    | otherwise                                 = Just flooding'
    where isUnderwater''     = isUnderwater' flooding' pos
          waterProofingLeft' = waterProofingLeft flooding'

updateWater :: forall s. MutableMine s -> Maybe (MutableMine s)
updateWater (mGrid, info) = do
    flooding' <- stepWaterProofing (stepWaterLevel (flooding info)) (robotPos (mGrid, info))
    return (mGrid, info{flooding = flooding'})