-- AI Stuff

> module AStar (
>     run
> ) where

> import Mine

manhattan distance = the taxicab metric
details @ http://en.wikipedia.org/wiki/Taxicab_geometry

> mdist :: Pos -> Pos -> Int
> mdist (x,y) (a,b) = abs (x-a) + abs (y-b)

Finds all the neighbours of a position. Note that this may
include positions which are outside of the map limits.

> neighbours :: Pos -> [Pos]
> neighbours (1,1) = [(2,1), (1,2)]
> neighbours (x,1) = [(x-1,1), (x+1,1), (x,2)]
> neighbours (1,y) = [(1,y-1), (1,y+1), (2,y)]
> neighbours (x,y) = [(x,y+1), (x+1,y), (x,y-1), (x-1,y)]

Given the dimension of a map and a list of positions, this
function removes all positions which are not actually on the map.

> limit :: (Int, Int) -> [Pos] -> [Pos]
> limit (n, m) ps = [(x, y) | (x, y) <- ps, not (n > x), not (m > y)]

Finds the positions adjacent to the robot's current position.

> surroundings :: Mine -> [Pos]
> surroundings m = limit (mineSize m) (neighbours (robotPos m))

> astar :: Mine -> Pos -> Pos -> [Pos] -> Path -> Path
> astar _ x y _ = 

> path :: Mine -> Pos -> Pos -> Path
> path m x y = undefined

> simulate :: Mine -> [Pos] -> [Path]
> simulate m = map (path m ??)

> search :: Mine -> [Path]
> search m = simulate m (findLambdas m)

> run :: Mine -> String
> run = showPath . choose . search
