-- AI Stuff

> module AStar (
>     run
> ) where

{----------------------------------------------------------------------}
{-- Module Imports                                                    -}
{----------------------------------------------------------------------}

> import Data.List (sort)
> import Mine
> import Core

{----------------------------------------------------------------------}
{-- Helper Functions                                                  -}
{----------------------------------------------------------------------}

manhattan distance = the taxicab metric
details @ http://en.wikipedia.org/wiki/Taxicab_geometry

> mdist :: Pos -> Pos -> Int
> mdist (Pos (x,y)) (Pos (a,b)) = abs (x-a) + abs (y-b)

Finds all the neighbours of a position. Note that this may
include positions which are outside of the map limits.

> neighbours :: Pos -> [Pos]
> neighbours (Pos (1,1)) = map Pos [(2,1), (1,2)]
> neighbours (Pos (x,1)) = map Pos [(x-1,1), (x+1,1), (x,2)]
> neighbours (Pos (1,y)) = map Pos [(1,y-1), (1,y+1), (2,y)]
> neighbours (Pos (x,y)) = map Pos [(x,y+1), (x+1,y), (x,y-1), (x-1,y)]

Given the dimension of a map and a list of positions, this
function removes all positions which are not actually on the map.

> limit :: (Int, Int) -> [Pos] -> [Pos]
> limit (n, m) ps = [Pos (x, y) | (Pos (x, y)) <- ps, not (n > x), not (m > y)]

Finds the positions adjacent to a position.

> surroundings :: Mine -> Pos -> [Pos]
> surroundings m p = limit (mineSize m) (neighbours p)

{----------------------------------------------------------------------}
{-- A* Search                                                         -}
{----------------------------------------------------------------------} 

> astar :: Mine -> [Pos] -> Pos -> [Pos] -> Path -> Path
> astar _ (x:_)  y _ _ | x == y = []
> astar m (x:xs) y c p          = surroundings m x

> path :: Mine -> Pos -> Pos -> Path
> path m x y = astar m [x] y [] []

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
> search m = simulate m $ findPaths m (robotPos m) (findLambdas m)

I would like more information than just a Path (i.e. the # of lambdas collected).Currently we only consider the length.

> choose :: [Path] -> Path
> choose ps = head $ sort [(length p,p) | p <- ps]

> run :: Mine -> String
> run = showPath . choose . search
