
module Flooding where

import Core
import Control.Monad.State

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

-- return Nothing if the robot drowned
stepWaterProofing :: FloodingState -> Pos -> Maybe FloodingState
stepWaterProofing flooding' pos
    | isUnderwater'' && waterProofingLeft' <= 0 = Nothing
    | isUnderwater''                            = Just (flooding'{waterProofingLeft = waterProofingLeft' - 1})
    | otherwise                                 = Just flooding'
    where isUnderwater''     = isUnderwater' flooding' pos
          waterProofingLeft' = waterProofingLeft flooding'

updateWater :: Mine -> Maybe Mine
updateWater mine = do
    flooding' <- stepWaterProofing (stepWaterLevel (flooding mine)) (robotPos mine)
    return (mine{flooding = flooding'})