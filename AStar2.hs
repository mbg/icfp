module Pathing where

-- most of an implementation of A*
-- shouldn't be too slow
-- it compiles
-- still some undefineds left that need replacing
-- also using a priority heap instead of the lists would be p cool
-- untested so certainly doesn't work yet
-- pretty much ripped directly from the wikipedia page
-- don't expect to see me before 10 tomorrow
-- love
-- david

import Control.Applicative ((<$>))
import Control.Monad (forM, filterM)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import Core
import Mine

type Heuristic = Pos -> Pos -> Float

data AStarState = AStarState
    { openSet   :: [Pos]
    , closedSet :: [Pos]
    , gScores   :: Map Pos Float
    , fScores   :: Map Pos Float
    , cameFrom  :: Map Pos Pos
    }

shortestPath :: Mine -> Heuristic -> Pos -> Pos -> Maybe [Cmd]
shortestPath mine heuristic start goal = if succeeded then Just cmds else Nothing
    where
    initState = AStarState [start] [] (M.singleton start 0) (M.singleton start (heuristic start goal)) M.empty
    (succeeded, finalState) = runState (shortestPath' mine heuristic goal) initState
    cmds = reconstructPath (cameFrom finalState) goal

-- this assumes that a path we've previously seen can only get worse
shortestPath' :: Mine -> Heuristic -> Pos -> State AStarState Bool
shortestPath' mine heuristic goal = do
    state <- get
    if null (openSet state)
      then return False
      else do
        let current = undefined -- we need the thing in the openSet with the smallest fScore
        if current == goal
        then return True
        else do
            removeFromOpenSetState current
            addToClosedSetState current
            closedSet' <- gets closedSet
            let neighbours' = filter (flip elem closedSet') (neighbours mine current)
            forM neighbours' $ \neighbour -> do
                openSet'        <- gets openSet
                tentativeGScore <- (+1) <$> getGScoreState current
                gScoreNeighbour <- getGScoreState neighbour
                when (notElem neighbour openSet' || tentativeGScore < gScoreNeighbour) $ do
                    addToOpenSetState neighbour
                    setCameFromState neighbour current
                    setGScoreState neighbour tentativeGScore
                    setFScoreState neighbour (tentativeGScore + heuristic neighbour goal)
            -- TODO: update the mine to allow neighbours to account for falling rocks and rising water
            shortestPath' mine heuristic goal
    where
    neighbours :: Mine -> Pos -> [Pos]
    neighbours = undefined

-- man, records suck so much
addToOpenSetState        x = modify (\state -> state{openSet = addSorted x (openSet state)})
addToClosedSetState      x = modify (\state -> state{closedSet = addSorted x (closedSet state)})
removeFromOpenSetState   x = modify (\state -> state{openSet = filter (/= x) (openSet state)})
removeFromClosedSetState x = modify (\state -> state{closedSet = filter (/= x) (closedSet state)})
setCameFromState key value = modify (\state -> state{cameFrom = M.insert key value (cameFrom state)})
setGScoreState   key value = modify (\state -> state{gScores = M.insert key value (gScores state)})
setFScoreState   key value = modify (\state -> state{fScores = M.insert key value (fScores state)})
getGScoreState   key       = gets (fromJust . M.lookup key . gScores)

-- how did I get here? Let's work backwards to find out
reconstructPath :: Map Pos Pos -> Pos -> [Cmd]
reconstructPath cameFrom goal = undefined

-- a poor man's substitute for a real priority queue
addSorted :: Ord a => a -> [a] -> [a]
addSorted x' [] = [x']
addSorted x' (x:xs) | x' <= x   = x':x:xs
                    | otherwise = x : addSorted x' xs