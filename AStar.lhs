-- AI Stuff

-- manhattan distance = the taxicab metric
-- details @ http://en.wikipedia.org/wiki/Taxicab_geometry
> mdist :: Pos -> Pos -> Int
> mdist (x,y) (a,b) = abs (x-a) + abs (y-b)

> neighbours :: Pos -> [Pos]
> neighbours (1,1) = [(2,1), (1,2)]
> neighbours (x,1) = [(x-1,1), (x+1,1), (x,2)]
> neighbours (1,y) = [(1,y-1), (1,y+1), (2,y)]
> neighbours (x,y) = [(x,y+1), (x+1,y), (x,y-1), (x-1,y)]

> limit :: (Int, Int) -> [Pos] -> [Pos]
> limit (n, m) ps = [(x, y) | (x, y) <- ps, not (n > x), not (m > y)]

> surroundings :: Mine -> [Pos]
> surroundings m = limit (mineSize m) (neighbours (robotPos m))

> astar :: Mine -> Pos -> Pos -> [Pos] -> Path -> Path

> path :: Mine -> Pos -> Pos -> Path
> path m x y = undefined

> run :: Mine -> String
> run = undefined
