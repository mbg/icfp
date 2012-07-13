module Mine where

import Prelude hiding (Either(..))

import Core
import Control.Monad.State

showMap :: Mine -> String
showMap = unlines . map (map toChar)
-- does NOT include the falling rocks, the main 
-- function will deal with this
moveRobot :: Cmd -> Mine -> Mine
moveRobot cmd mn | isValidMove   mn cmd && 
                   rockNeedsPushing mn cmd = let rPos = robotPos mn
                                                 rnewPos = move rPos cmd 
                                                 rockPos = move rnewPos cmd in
                         setObj Rock (setObj Robot (setObj Empty mn rPos) rnewPos) rockPos
                 | isValidMove mn cmd = let rPos     = robotPos mn
                                            rnewPos  = move rPos cmd in
                         setObj Robot (setObj Empty mn rPos) rnewPos 
                 | otherwise          = mn 

isValidMove :: Mine -> Cmd -> Bool
isValidMove mine cmd | objAt mine (move robot cmd) `elem` 
                       [Empty, Earth, Lambda, OpenLift] 
                          = True
                     | rockNeedsPushing mine cmd
                          = True
                       where robot = robotPos mine
isValidMove _ _ = False

rockNeedsPushing :: Mine -> Cmd -> Bool
rockNeedsPushing mine cmd
    | cmd `elem` [Left, Right] && 
      objAt mine (move robot cmd) == Rock && 
      objAt mine (move (move robot cmd) cmd) == Empty 
        = True
    | otherwise
        = False
    where robot = robotPos mine

-- if we move onto an open lift, we win
isWinningMove :: Cmd -> Mine -> Bool
isWinningMove cmd mine = objAt mine (move (robotPos mine) cmd) == OpenLift

-- if we update the state and then have a rock above us, we lose
willThisMoveKillUs :: Cmd -> Mine -> Bool
willThisMoveKillUs cmd mine | not (isWinningMove cmd mine)
    = objAt mine' (move (robotPos mine) Up) == Rock
    where mine' = moveRocks (moveRobot cmd mine)

updateMine :: Cmd -> Mine -> Mine
updateMine cmd = updateLifts . moveRocks . moveRobot cmd

updateLifts :: Mine -> Mine
-- XXX: slowish with lists
updateLifts mine | noLambdas mine = foldl (setObj OpenLift) 
                                    mine (objPos ClosedLift mine)
                 | otherwise      = mine

moveRocks :: Mine -> Mine
--- XXX: slowish with lists
moveRocks mine = foldl (setObj Rock) mine newRocks
    where
    oldRocks = rockPos mine
    newRocks = map (newRockPos mine) oldRocks
    mine'    = foldl (setObj Empty) mine oldRocks

setObj :: Obj -> Mine -> Pos -> Mine
setObj obj mine pos = map (map setObjCell) (numberMine mine)
    where setObjCell (pos', obj') | pos' == pos = obj
                                  | otherwise   = obj'

numberMine :: Mine -> [[(Pos, Obj)]]
numberMine mine = [[((x,y), obj) | (x, obj) <- zip [1..] row] | (y, row) <- zip [1..] mine]

newRockPos :: Mine -> Pos -> Pos
-- assumes that there is a rock at oldPos
newRockPos mine (x, y)
    | objAt' down      == Empty = down
    | objAt' down      == Rock &&
      objAt' right     == Empty &&
      objAt' downRight == Empty = downRight
    | objAt' down      == Rock &&
      objAt' left      == Empty &&
      objAt' downLeft  == Empty = downLeft
    | objAt' down      == Lambda &&
      objAt' right     == Empty &&
      objAt' downRight == Empty = downRight
    | otherwise                 = (x, y)
    where objAt'    = objAt mine
          down      = (x  , y-1)
          downLeft  = (x-1, y-1)
          downRight = (x+1, y-1)
          left      = (x-1, y)
          right     = (x+1, y)

-- down with lambdas, up with lifting
noLambdas :: Mine -> Bool
noLambdas = all (all (/= Lambda))

mineHeight :: Mine -> Int
mineHeight = length

mineWidth :: Mine -> Int
mineWidth = length . head

mineSize :: Mine -> (Int, Int)
mineSize m = (mineWidth m, mineHeight m)

robotPos :: Mine -> Pos
-- XXX: if there is no robot, CRASH
robotPos = head . objPos Robot

rockPos :: Mine -> [Pos]
rockPos = objPos Rock

objPos :: Obj -> Mine -> [Pos]
objPos obj = map fst . filter (\(pos, obj') -> obj == obj') . concat . numberMine

objAt :: Mine -> Pos -> Obj
objAt mine (x,y) = (mine !! (y - 1)) !! (x - 1)
