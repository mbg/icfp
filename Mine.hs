module Mine where

import Data.Array.IArray
import Data.Tuple (swap)
import Prelude hiding (Either(..))

import Core

-- TODO: padding
readMine :: String -> Mine
readMine str = listArray (Pos (1,1), Pos (maxX, maxY)) (concatMap (pad maxX . map toObj) rows)
    where
    rows = lines str
    maxY = length rows
    maxX = maximum (map length rows)
    pad :: Int -> [Obj] -> [Obj]
    pad n xs | len < n   = xs ++ replicate (n - len) Empty
             | otherwise = xs
        where len = length xs

showMap :: Mine -> String
showMap mine = intersperseEvery '\n' width . map toChar . elems $ mine
    where
    intersperseEvery :: a -> Int -> [a] -> [a]
    intersperseEvery x n xs = let (pre, post) = splitAt n xs in pre ++ (x:post) ++ intersperseEvery x n post
    (width, _) = mineSize mine

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
-- XXX: EXCEPT IF ROCK HASN'T MOVED
willThisMoveKillUs :: Cmd -> Mine -> Bool
willThisMoveKillUs cmd mine | not (isWinningMove cmd mine)
    = objAt mine' (move (robotPos mine) Up) == Rock
    where mine' = moveRocks (moveRobot cmd mine)

updateMine :: Cmd -> Mine -> Mine
updateMine cmd = updateLifts . moveRocks . moveRobot cmd

updateLifts :: Mine -> Mine
updateLifts mine | noLambdas mine = foldl (setObj OpenLift) 
                                    mine (objPos ClosedLift mine)
                 | otherwise      = mine

moveRocks :: Mine -> Mine
moveRocks mine = foldl (setObj Rock) mine newRocks
    where
    oldRocks = rockPos mine
    newRocks = map (newRockPos mine) oldRocks
    mine'    = foldl (setObj Empty) mine oldRocks

setObj :: Obj -> Mine -> Pos -> Mine
setObj obj mine pos = listArray (bounds mine) . map setObjCell . assocs $ mine
    where setObjCell (pos', obj') | pos' == pos = obj
                                  | otherwise   = obj'

newRockPos :: Mine -> Pos -> Pos
-- assumes that there is a rock at oldPos
newRockPos mine pos
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
    | otherwise                 = pos
    where objAt'    = objAt mine
          down      = move pos Down
          downLeft  = move (move pos Down) Left
          downRight = move (move pos Down) Right
          left      = move pos Left
          right     = move pos Right

-- down with lambdas, up with lifting
noLambdas :: Mine -> Bool
noLambdas = all (/= Lambda) . elems

mineSize :: Mine -> (Int, Int)
mineSize = unPos . snd .  bounds

findLambdas :: Mine -> [Pos]
findLambdas = objPos Lambda

robotPos :: Mine -> Pos
-- XXX: if there is no robot, CRASH
robotPos = head . objPos Robot

rockPos :: Mine -> [Pos]
rockPos = objPos Rock

objPos :: Obj -> Mine -> [Pos]
objPos obj = map fst . filter (\(pos, obj') -> obj == obj') . assocs

objAt :: Mine -> Pos -> Obj
objAt = (!)

locateLambdas :: Mine -> [Pos]
locateLambdas = objPos Lambda



