
> module MCS (
>   run
> ) where

{----------------------------------------------------------------------}
{-- Module Imports                                                    -}
{----------------------------------------------------------------------}

> import Control.Monad.State
> import Data.List (sort)
> import qualified Data.PQueue.Min as PQ
> import Debug.Trace (trace)
> import Control.Parallel

> import Core
> import Mine
> import AStar

{----------------------------------------------------------------------}
{-- Minimum Cost Search                                               -}
{----------------------------------------------------------------------}

> hasOpenLift :: Mine -> Bool
> hasOpenLift m = not $ null $ objPos OpenLift m

> data SearchNode = SN {
>   nodeMine :: Mine,
>   nodePath :: Path
> } deriving Show

> instance Eq SearchNode where
>   x == y = nodePath x == nodePath y

> instance Ord SearchNode where
>   compare x y = length (nodePath x) `compare` length (nodePath y)

> data SearchState = SS {
>   open   :: PQ.MinQueue SearchNode,
>   aborts :: [Path]
> }

> type MCS = State SearchState

> initMCS :: Mine -> SearchState
> initMCS m = SS (PQ.singleton (SN m [])) []

> nextNode :: MCS SearchNode
> nextNode =  do 
>   st <- get
>   put $ st { open = PQ.deleteMin (open st)}
>   return $ PQ.findMin (open st)

> addAbort :: Path -> MCS ()
> addAbort p = modify $ \s -> s { aborts = p : aborts s}

> addOpen :: SearchNode -> MCS ()
> addOpen n = modify $ \s -> s { open = PQ.insert n (open s) } 

> addOpens :: [SearchNode] -> MCS ()
> addOpens = mapM_ addOpen  

> findLambdaPaths :: Mine -> [(Path,Mine)]
> findLambdaPaths m = map (path m p) ps
>                     where
>                       p  = robotPos m
>                       ps = objPos Lambda m

> findLiftPath :: Mine -> (Path,Mine)
> findLiftPath m = path m (robotPos m) (findLambdaLift m)

> makeNode :: Path -> (Path,Mine) -> SearchNode
> makeNode cs (p,m) = SN m (cs ++ p)

> makeNodes :: SearchNode -> [(Path,Mine)] -> [SearchNode]
> makeNodes (SN _ p) = map (makeNode p)

> returnAbort :: MCS Path
> returnAbort = do
>    al <- aborts `fmap` get
>    if null al 
>    then trace ("I am returning abort because the abort list is empty") (return [Abort])
>    else return $ snd $ head $ sort [(length p, p) | p <- al]

> isGoal :: Mine -> Bool
> isGoal m = case finishedScore m of
>   (Just _) -> True
>   Nothing  -> False

> mcs' :: MCS Path
> mcs' = do
>   ol <- open `fmap` get
>   if PQ.null ol
>   then returnAbort
>   else do
>     n <- nextNode
>     if isGoal (nodeMine n)
>     then return $ nodePath n
>     else if Abort `elem` nodePath n
>       then addAbort (nodePath n) >> mcs'
>       else if {-trace (show n)-} (hasOpenLift (nodeMine n))
>          then do
>            addOpen $ makeNode (nodePath n) $ findLiftPath (nodeMine n)
>            mcs'
>          else do
>            addOpens $ makeNodes n $ findLambdaPaths (nodeMine n)
>            mcs'

> mcs :: Mine -> Path
> mcs m = evalState mcs' (initMCS m)





> simulate :: (Path,Mine) -> Path
> simulate (p,m) | Abort `elem` p = p
>                | otherwise      = p ++ (choose . search) m

> simulatePaths :: [(Path,Mine)] -> [Path]
> simulatePaths ps = map simulate ps

> findLambdaLift :: Mine -> Pos
> findLambdaLift m = head (objPos OpenLift m)

> findPaths :: Mine -> Pos -> [Pos] -> [(Path,Mine)]
> findPaths m p [] | hasOpenLift m = [path m p (findLambdaLift m)]
>                  | otherwise     = []
> findPaths m p ps                 = map (path m p) ps

> search :: Mine -> [Path]
> search m = simulatePaths $ findPaths m (robotPos m) (objPos Lambda m)

I would like more information than just a Path (i.e. the # of lambdas collected).Currently we only consider the length.

> choose :: [Path] -> Path
> choose [] = []
> choose ps = snd . head $ sort [(length p,p) | p <- ps]

> run :: Mine -> String
> run = showPath . mcs

run = showPath . choose . search
