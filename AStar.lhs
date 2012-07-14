-- AI Stuff

> module AStar (
>     run
> ) where

{----------------------------------------------------------------------}
{-- Module Imports                                                    -}
{----------------------------------------------------------------------}

Data.PQueue.Min requires 'cabal install pqueue'.

> import Prelude hiding (Either(..))
> import Control.Monad.State
> import Data.List (sort)
> import qualified Data.PQueue.Min as PQ
> import Mine
> import Core

{----------------------------------------------------------------------}
{-- Helper Functions                                                  -}
{----------------------------------------------------------------------}

XXX: can we factor in the flooding rates to the A* sourcing metric?

manhattan distance = the taxicab metric
details @ http://en.wikipedia.org/wiki/Taxicab_geometry

> mdist :: Pos -> Pos -> Int
> mdist (Pos (x,y)) (Pos (a,b)) = abs (x-a) + abs (y-b)

Finds the positions adjacent to a position. We remove illegal positions from this list.

XXX: doesn't consider a robot pushing rocks

> surroundings :: Mine -> Pos -> [Pos]
> surroundings m p = map (move p) allowedCmds
>    where allowedCmds = filter allowedCmd dirs
>          allowedCmd cmd = not (isLosingMove newMine cmd) && isValidMove newMine cmd
>          newMine = setRobotPos m p
           

> action :: Pos -> Pos -> Cmd
> action (Pos (x,y)) (Pos (a,b)) 
>     | x < a = Right
>     | x > a = Left
>     | y < b = Up
>     | y > b = Down

{----------------------------------------------------------------------}
{-- A* Search                                                         -}
{----------------------------------------------------------------------} 

A node in the search tree represent a position in the mine. We also
store information about where we came from and what t

g is the cost of the path so far
h is the heuristic value
f is the result of g+h

> data SearchNode = SN {
>   previousNode :: Maybe SearchNode,
>   nodeAction   :: Maybe Cmd,
>   nodePos      :: Pos,
>   nodeG        :: Int,
>   nodeH        :: Int
> }

Two nodes are considered equal if they have the same position.

> instance Eq SearchNode where
>     x == y = nodePos x == nodePos y

We can order search nodes by their f value.

> instance Ord SearchNode where
>     compare x y = compare (nodeF x) (nodeF y)

> nodeF :: SearchNode -> Int
> nodeF (SN _ _ _ g h) = g + h

> isTarget :: SearchNode -> Pos -> Bool
> isTarget n p = nodePos n == p

> data SearchState = SS {
>   mine   :: Mine,
>   closed :: [Pos],
>   open   :: PQ.MinQueue SearchNode
> }

> initSearchState :: Mine -> Pos -> Pos -> SearchState
> initSearchState m o p = SS m [] (PQ.singleton n)
>                         where
>                             h = mdist o p 
>                             n = SN Nothing Nothing p 0 h

> getMine :: AStar Mine
> getMine = mine `fmap` get

> type AStar = State SearchState

> nextNode :: AStar SearchNode
> nextNode = (PQ.findMin . open) `fmap` get

> addClosed :: SearchNode -> AStar ()
> addClosed n = modify $ \s -> s { closed = nodePos n : closed s } 

> addOpen :: SearchNode -> AStar ()
> addOpen n = modify $ \s -> s { open = PQ.insert n (open s) } 

> makeNode :: SearchNode -> Int -> Pos -> Pos -> SearchNode
> makeNode n g o d = SN (Just n) (Just a) d g (mdist o d)
>                    where
>                        a = action (nodePos n) d

> addOpens :: Pos -> Int -> SearchNode -> [Pos] -> AStar ()
> addOpens o g p ps = mapM_ addOpen $ map (makeNode p g o) ps       

> followPath :: Maybe SearchNode -> Path -> Path
> followPath (Just n) ps = constructPath n ps
> followPath Nothing  ps = ps

p needs to be a Cmd

> constructPath :: SearchNode -> Path -> Path
> constructPath (SN n a _ _ _) ps = case a of
>     (Just c) -> followPath n (c : ps)
>     Nothing  -> ps

> astar :: Pos -> Pos -> AStar Path
> astar o t = do
>   n <- nextNode
>   if isTarget n t 
>   then return $ constructPath n []
>   else do
>       addClosed n
>       m <- getMine
>       addOpens o (nodeG n + 1) n $ surroundings m (nodePos n) 
>       astar o t

> path :: Mine -> Pos -> Pos -> Path
> path m x y = evalState (astar x y) (initSearchState m x y)

{----------------------------------------------------------------------}
{-- Main Search Algorithm                                             -}
{----------------------------------------------------------------------}

If a step doesn't work because a rock is in the way or the player would get crushed, we need to run A* again

> simulate :: Mine -> Path -> Path
> simulate m []     = (choose . search) m
> simulate m (x:xs) = x : simulate (updateMine x m) xs

> simulatePaths :: Mine -> [Path] -> [Path]
> simulatePaths m ps = map (simulate m) ps

> findPaths :: Mine -> Pos -> [Pos] -> [Path]
> findPaths m p ps = map (path m p) ps

> search :: Mine -> [Path]
> search m = simulatePaths m $ findPaths m (robotPos m) (objPos Lambda m)

I would like more information than just a Path (i.e. the # of lambdas collected).Currently we only consider the length.

> choose :: [Path] -> Path
> choose ps = snd . head $ sort [(length p,p) | p <- ps]

> run :: Mine -> String
> run = showPath . choose . search

