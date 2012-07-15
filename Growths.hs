
module Growths where

import Core

makeBeardGrowth :: Int -> Int -> BeardGrowth
makeBeardGrowth startRazors growRate = BeardGrowth startRazors growRate (growRate - 1)

cardinals :: Mine -> Pos -> [Pos]
cardinals mn (Pos x y) = filter (inScope (mineSize mn)) [ Pos (x - 1) (y - 1)
                                                        , Pos x       (y - 1)
                                                        , Pos (x + 1) (y - 1) 
                                                        , Pos (x - 1) y
                                                        , Pos (x + 1) y
                                                        , Pos (x - 1) (y + 1)
                                                        , Pos x       (y + 1)
                                                        , Pos (x + 1) (y + 1) ]
                         where inScope :: (Int, Int) -> Pos -> Bool
                               inScope (n, m) (Pos x y) |  x < 1 || x > n 
                                                        || y < 1 || y > m = False
                                                        |  otherwise      = True                    
                     
applyRazor :: Mine -> Mine
applyRazor mn = let rPos = robotPos mn
                    region = cardinals mn rPos in
                multi Empty mn region
                where multi _ m []       = m
                      multi obj m (p:ps) = multi obj (setObj obj m p) ps
                
updateBeards :: Mine -> Mine
updateBeards = undefined