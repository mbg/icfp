
> module MCS (
>   run
> ) where

{----------------------------------------------------------------------}
{-- Module Imports                                                    -}
{----------------------------------------------------------------------}

> import Control.Monad.State
> import Data.List (sort)
> import qualified Data.PQueue.Min as PQ

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
> }

> instance Eq SearchNode where
>   x == y = nodePath x == nodePath y

> instance Ord SearchNode where
>   compare x y = length (nodePath x) `compare` length (nodePath y)

> data SearchState = SS {
>   open :: PQ.MinQueue SearchNode
> }

> type MCS = State SearchState

> initMCS :: Mine -> SearchState
> initMCS m = SS $ PQ.singleton (SN m [])

> nextNode :: MCS SearchNode
> nextNode =  do 
>   st <- get
>   put $ st { open = PQ.deleteMin (open st)}
>   return $ PQ.findMin (open st)

> addOpen :: SearchNode -> MCS ()
> addOpen n = modify $ \s -> s { open = PQ.insert n (open s) } 

> addOpens :: [SearchNode] -> MCS ()
> addOpens = mapM_ addOpen  

> findLambdaPaths :: Mine -> [Path]
> findLambdaPaths m = map (path m p) ps
>                     where
>                       p  = robotPos m
>                       ps = objPos Lambda m

> mcs' :: MCS Path
> mcs' = do
>   n <- nextNode
>   if hasOpenLift (nodeMine n) 
>   then return [] 
>   else do
>       --ps <- findLambdaPaths (nodeMine n)
>       return []

> mcs :: Mine -> Path
> mcs m = evalState mcs' (initMCS m)




If a step doesn't work because a rock is in the way or the player would get crushed, we need to run A* again

> simulate :: Mine -> Path -> Path
> simulate m []                       = (choose . search) m
> simulate m (Abort:_)                = [Abort]
> simulate m (x:xs) | isValidMove m x = x : simulate (updateMine x m) xs
>                   | otherwise       = (choose . search) m

> simulatePaths :: Mine -> [Path] -> [Path]
> simulatePaths m ps = map (simulate m) ps

> findLambdaLift :: Mine -> Pos
> findLambdaLift m = head (objPos OpenLift m)

> findPaths :: Mine -> Pos -> [Pos] -> [Path]
> findPaths m p [] | hasOpenLift m = [path m p (findLambdaLift m)]
>                  | otherwise     = []
> findPaths m p ps                 = map (path m p) ps

> search :: Mine -> [Path]
> search m = simulatePaths m $ findPaths m (robotPos m) (objPos Lambda m)

I would like more information than just a Path (i.e. the # of lambdas collected).Currently we only consider the length.

> choose :: [Path] -> Path
> choose [] = []
> choose ps = snd . head $ sort [(length p,p) | p <- ps]

> run :: Mine -> String
> run = showPath . choose . search