module Mine where

import Data.Array.IArray
import Data.Maybe (isJust)
import Prelude hiding (Either(..))

import Core

readMine :: String -> Mine
readMine str = listArray (Pos (1,1), Pos (maxX, maxY)) (concatMap (pad maxX . map toObj) rows)
    where
    rows = reverse (lines str)
    maxY = length rows
    maxX = maximum (map length rows)
    pad :: Int -> [Obj] -> [Obj]
    pad n xs | len < n   = xs ++ replicate (n - len) Empty
             | otherwise = xs
        where len = length xs

showMine :: Mine -> String
showMine mine = unlines . reverse . splitAtEvery width . map toChar . elems $ mine
    where
    splitAtEvery :: Int -> [a] -> [[a]]
    splitAtEvery _ [] = []
    splitAtEvery n xs = let (x,xs') = splitAt n xs in x : splitAtEvery n xs'
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
isValidMove mine cmd
    | objAt mine (move robot cmd) `elem`
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

-- if we update the state and then have a rock above us
-- that wasn't there before, we lose
isLosingMove :: Cmd -> Mine -> Bool
isLosingMove cmd mine | not (isWinningMove cmd mine)
    = objAt mine' (move (robotPos mine') Up) == Rock &&
      objAt mine  (move (robotPos mine') Up) /= Rock
    where mine' = fst . moveRocks. moveRobot cmd $ mine

updateMine :: Cmd -> Mine -> Mine
updateMine cmd = updateLifts . fst . moveRocks . moveRobot cmd

updateLifts :: Mine -> Mine
updateLifts mine | noLambdas mine = foldl (setObj OpenLift)
                                    mine (objPos ClosedLift mine)
                 | otherwise      = mine

-- moves the rocks in the mine and also returns if it actually moved any
moveRocks :: Mine -> (Mine, Bool)
moveRocks mine | not (any (isJust . fst) newOldPairs) = (mine  , False)
               | otherwise                            = (mine'', True )
    where
    newOldPairs :: [(Maybe Pos, Pos)]
    newOldPairs  = map (\pos -> (newRockPos mine pos, pos)) (rockPos mine)
    mine'        = foldl maybeEmptyOld mine  newOldPairs
    mine''       = foldl maybeReplaceNew mine' newOldPairs

    maybeEmptyOld :: Mine -> (Maybe Pos, Pos) -> Mine
    maybeEmptyOld mine (Nothing, _  )   = mine
    maybeEmptyOld mine (Just _ , old)   = setObj Empty mine old

    maybeReplaceNew :: Mine -> (Maybe Pos, Pos) -> Mine
    maybeReplaceNew mine (Nothing , _) = mine
    maybeReplaceNew mine (Just new, _) = setObj Rock mine new

setObj :: Obj -> Mine -> Pos -> Mine
setObj obj mine pos = listArray (bounds mine) . map setObjCell . assocs $ mine
    where setObjCell (pos', obj') | pos' == pos = obj
                                  | otherwise   = obj'

newRockPos :: Mine -> Pos -> Maybe Pos
-- assumes that there is a rock at oldPos
-- if the rock doesn't move, return Nothing
newRockPos mine pos
    | objAt' down      == Empty = Just down
    | objAt' down      == Rock &&
      objAt' right     == Empty &&
      objAt' downRight == Empty = Just downRight
    | objAt' down      == Rock &&
      objAt' left      == Empty &&
      objAt' downLeft  == Empty = Just downLeft
    | objAt' down      == Lambda &&
      objAt' right     == Empty &&
      objAt' downRight == Empty = Just downRight
    | otherwise                 = Nothing
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

