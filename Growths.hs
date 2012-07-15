
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
                                                                            
nesw :: Mine -> Pos -> [Pos]
nesw mn (Pos x y) = filter (inScope (mineSize mn)) [ Pos x       (y - 1)
                                                   , Pos (x - 1)  y
                                                   , Pos (x + 1)  y
                                                   , Pos x       (y + 1) ]
                         where inScope :: (Int, Int) -> Pos -> Bool
                               inScope (n, m) (Pos x y) |  x < 1 || x > n 
                                                        || y < 1 || y > m = False
                                                        |  otherwise      = True     
                     
applyRazor :: Mine -> Mine
applyRazor mn = let bd = beardData mn
                    numRaz = numberRazors bd
                    rPos = robotPos mn
                    region = cardinals mn rPos in
                case numRaz > 0 of
                  True  -> (multi Empty mn (filter (\p -> objAt mn p == Beard) region)) 
                           { beardData = bd { numberRazors = numRaz - 1 } }
                  False -> mn
                where multi :: Obj -> Mine -> [Pos] -> Mine
                      multi _ m []       = m
                      multi obj m (p:ps) = multi obj (setObj obj m p) ps
                      
updateBeards :: Mine -> Mine
updateBeards mn = let bg    = beardData mn in
                  case (stepsToGrowth bg) - 1 == 0 of
                    True  -> growBeards mn
                    False -> mn { beardData = bg { stepsToGrowth = stepsToGrowth bg - 1 } }

growBeards :: Mine -> Mine
growBeards mn = let bg = beardData mn 
                    bgr = beardGrowthRate bg
                    beardRegions = map (cardinals mn) (beardLocs mn) in 
                (multim mn ( map (filter (\p -> objAt mn p == Empty)) beardRegions ) ) 
                { beardData = bg { stepsToGrowth = bgr - 1 } }
                where multim :: Mine -> [[Pos]] -> Mine
                      multim m []       = m
                      multim m (p:ps)   = multim (multi m p) ps
                      multi :: Mine -> [Pos] -> Mine
                      multi m []        = m
                      multi m (p:ps)    = multi (setObj Beard m p) ps
                      
                                                                
beardsNearby :: Mine -> Int
beardsNearby = \m -> length $ filter (\p -> objAt m p == Beard) (nesw m (robotPos m))

