-- AI Stuff

> module AStar (
>     path
> ) where

{----------------------------------------------------------------------}
{-- Module Imports                                                    -}
{----------------------------------------------------------------------}

Data.PQueue.Min requires 'cabal install pqueue'.

> import Prelude hiding (Either(..))
> import Control.Monad.State
> import Data.Array.IArray (bounds)
> import Data.Ix (inRange)
> import Data.List (sort)
> import qualified Data.PQueue.Min as PQ
> import Mine
> import Core
> import Debug.Trace (trace)

{----------------------------------------------------------------------}
{-- Helper Functions                                                  -}
{----------------------------------------------------------------------}

XXX: can we factor in the flooding rates to the A* sourcing metric?

manhattan distance = the taxicab metric
details @ http://en.wikipedia.org/wiki/Taxicab_geometry

> mdist :: Pos -> Pos -> Int
> mdist (Pos x y) (Pos a b) = abs (x-a) + abs (y-b)

Finds the positions adjacent to a position. We remove illegal positions from this list.

> {-# DEPRECATED surroundings "We should be using nextPossibleStates instead!" #-}
> surroundings :: Mine -> Pos -> [Pos]
> surroundings m p = map (move p) notLosingCmds
>    where notLosingCmds = filter (\cmd -> isValidMove newMine cmd && not (isLosingMove newMine cmd)) dirs
>          newMine       = setRobotPos m p
           
> nextPossibleStates :: Mine -> [Mine]
> nextPossibleStates mn = map (flip updateMine mn) moves
>     where moves = filter (\cmd -> isValidMove mn cmd && not (isLosingMove mn cmd)) dirs

> action :: Pos -> Pos -> Cmd
> action (Pos x y) (Pos a b) 
>     | x < a            = Right
>     | x > a            = Left
>     | y < b            = Up
>     | y > b            = Down
>     | x == a && y == b = Wait

{----------------------------------------------------------------------}
{-- A* Search                                                         -}
{----------------------------------------------------------------------} 

A node in the search tree represent a position in the mine. We also
store information about where we came from and what t

g is the cost of the path so far
h is the heuristic value
f is the result of g+h

> data SearchNode = SN {
>   nodeMine     :: Mine,
>   previousNode :: Maybe SearchNode,
>   nodeAction   :: Maybe Cmd,
>   nodePos      :: !Pos,
>   nodeG        :: {-# UNPACK #-} !Int,
>   nodeH        :: {-# UNPACK #-} !Int
> } deriving Show

Two nodes are considered equal if they have the same position.

> instance Eq SearchNode where
>     x == y = nodePos x == nodePos y

We can order search nodes by their f value.

> instance Ord SearchNode where
>     compare x y = compare (nodeF x) (nodeF y)

> nodeF :: SearchNode -> Int
> nodeF (SN _ _ _ _ g h) = g + h

> isTarget :: SearchNode -> Pos -> Bool
> isTarget n p = nodePos n == p

> data SearchState = SS {
>   closed :: [Pos],
>   open   :: PQ.MinQueue SearchNode
> }

> initSearchState :: Mine -> Pos -> Pos -> SearchState
> initSearchState m o p = SS [] (PQ.singleton n)
>                         where
>                             h = mdist o p 
>                             n = SN m Nothing Nothing o 0 h

> type AStar = State SearchState

> query :: MonadState s m => (s -> s) -> m s
> query f = get >>= \v -> put (f v) >> return v

> nextNode :: AStar SearchNode
> nextNode =  do st <- get
>                put $ st { open = PQ.deleteMin (open st)}
>                return $ PQ.findMin (open st)

> addClosed :: SearchNode -> AStar ()
> addClosed n = modify $ \s -> s { closed = nodePos n : closed s } 

> addOpen :: SearchNode -> AStar ()
> addOpen n = do s <- get
>                if nodePos n `elem` closed s
>                then return ()
>                else put $ s { open = PQ.insert n (open s) } 

> makeNode :: SearchNode -> Int -> Pos -> Mine -> SearchNode
> makeNode n g o m = SN m (Just n) (Just a) d g (mdist o d)
>                    where
>		         d = robotPos m
>                        a = action (nodePos n) d

> addOpens :: Pos -> Int -> SearchNode -> [Mine] -> AStar ()
> addOpens o g p ps = mapM_ addOpen $ map (makeNode p g o) ps       

> followPath :: Maybe SearchNode -> Path -> Path
> followPath (Just n) ps = constructPath n ps
> followPath Nothing  ps = ps



> constructPath :: SearchNode -> Path -> Path
> constructPath (SN m n a _ _ _) ps = case a of
>     (Just c) -> followPath n (c : ps)
>     Nothing  -> ps

> astar :: Pos -> Pos -> AStar (Path, Mine)
> astar o t = do
>   n <- nextNode
>   if isTarget n t 
>   then return $ (constructPath n [], nodeMine n)
>   else do
>       addClosed n
>       addOpens o (nodeG n + 1) n $ nextPossibleStates (nodeMine n) 
>       ol <- open `fmap` get
>       if PQ.null ol 
>       then return $ (constructPath n [Abort], nodeMine n)
>       else astar o t

> path :: Mine -> Pos -> Pos -> (Path, Mine)
> path m x y = evalState (astar x y) (initSearchState m x y)

