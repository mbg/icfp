{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module Mine where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Array.IArray
import Data.Maybe (isJust, catMaybes, fromMaybe, fromJust)
import Debug.Trace
import Prelude hiding (Either(..))
import Debug.Trace (trace)

import Core
import Flooding
import Trampolines
import Growths

readMine :: String -> Mine
readMine str = Mine { grid = listArray bounds (concatMap (pad maxX . map toObj) (reverse rows))
                    , flooding = parseFlooding metaData
                    , beardData = undefined             
                    , trampolines = parseTrampolines metaData
                    , stepsTaken = 0
                    , lambdasCollected = 0}
    where
    bounds = (Pos 1 1, Pos maxX maxY)
    (rows, metaData) = break null (lines str)
    maxY = length rows
    maxX = maximum (map length rows)
    pad :: Int -> [Obj] -> [Obj]
    pad n xs = take n (xs ++ repeat Empty)

parseFlooding :: [String] -> FloodingState
parseFlooding css = fromMaybe defaultFlooding (makeFloodingState <$> level' <*> flooding' <*> waterproof')
    where level' = msum (map (\cs -> case cs of
              'W':'a':'t':'e':'r':' ':cs' -> Just (read cs')
              _ -> Nothing) css)
          flooding' = msum (map (\cs -> case cs of
              'F':'l':'o':'o':'d':'i':'n':'g':' ':cs' -> Just (read cs')
              _ -> Nothing) css)
          waterproof' = msum (map (\cs -> case cs of
              'W':'a':'t':'e':'r':'p':'r':'o':'o':'f':' ':cs' -> Just (read cs')
              _ -> Nothing) css)

parseTrampolines :: [String] -> [(Char, Char)]
parseTrampolines =  catMaybes . map (\cs -> case cs of
    'T':'r':'a':'m':'p':'o':'l':'i':'n':'e':' ':tramp:' ':'t':'a':'r':'g':'e':'t':'s':' ':target:_ -> Just (tramp, target)
    _ -> Nothing)

-- does NOT include the falling rocks, the main
-- function will deal with this
moveRobot :: Cmd -> Mine -> Mine
moveRobot cmd mn | valid && isTrampoline obj = let (Target c) = objAt mn jumpDest' in mapObjs (removeTrampolines (toThisTarget c mn)) (setRobotPos mn jumpDest')
    -- Empty where robot was, Robot where target x is, Empty where all old trampoline x
                 | valid && rockNeedsPushing mn cmd = let !rockMovedMine = moveObj mn newRobot newRock in moveObj rockMovedMine oldRobot newRobot
    -- move the rock then move the robot
                 | valid = moveObj mn oldRobot newRobot
    -- just move the robot
                 | otherwise          = mn
    where valid = isValidMove mn cmd
          oldRobot = robotPos mn
          newRobot = move oldRobot cmd
          newRock  = move newRobot cmd
          obj = objAt mn newRobot
          jumpDest' = jumpDest mn newRobot
          
isValidMove :: Mine -> Cmd -> Bool
isValidMove mine cmd
    | not (inRange (bounds (grid mine)) (move robot cmd)) = False
    | (obj `elem` [Empty, Earth, Lambda, OpenLift]) ||
      isTrampoline obj
        = True
    | rockNeedsPushing mine cmd
        = True
    where robot = robotPos mine
          obj = objAt mine (move robot cmd)
isValidMove _ _ = False

-- move an object from its old position to a new position and leave Empty behind
moveObj :: Mine -> Pos -> Pos -> Mine
moveObj mine old new = setObj (objAt mine old) (setObj Empty mine old) new

setRobotPos :: Mine -> Pos -> Mine
setRobotPos mine = moveObj mine (robotPos mine)

rockNeedsPushing :: Mine -> Cmd -> Bool
rockNeedsPushing mine cmd
    | cmd `elem` [Left, Right] &&
      objAt mine (move robot cmd) == Rock &&
      inRange (bounds (grid mine)) (move (move robot cmd) cmd) &&
      objAt mine (move (move robot cmd) cmd) == Empty
        = True
    | otherwise
        = False
    where robot = robotPos mine

-- if we move onto an open lift, we win
isWinningMove :: Mine -> Cmd -> Bool
isWinningMove mine cmd = objAt mine (move (robotPos mine) cmd) == OpenLift

-- if we update the state and then have a rock above us
-- that wasn't there before or we drown, we lose
isLosingMove :: Mine -> Cmd -> Bool
isLosingMove mine cmd =
    (objAt mine' (move (robotPos mine') Up) == Rock &&
     objAt mine  (move (robotPos mine') Up) /= Rock) ||
    (robotDrowned (updateEnv mine))
    where mine' = fst . moveRocks. moveRobot cmd $ mine

updateEnv :: Mine -> Mine
updateEnv = openLiftH . fst . moveRocks . updateWater . updateBeards

updateMine :: Cmd -> Mine -> Mine
updateMine cmd = updateEnv . moveRobot cmd

mapObjs :: (Obj -> Obj) -> Mine -> Mine
mapObjs f mine = mine {grid = f <$> grid mine}

openLiftH :: Mine -> Mine
openLiftH m | noLambdas m = mapObjs openLift m
            | otherwise   = m

openLift :: Obj -> Obj
openLift ClosedLift = OpenLift
openLift obj        = obj

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
setObj obj mine pos = mine{grid = array bounds' . map setObjCell . assocs $ (grid mine)}
    where bounds' = bounds (grid mine)
          setObjCell (pos',obj') | pos' == pos = (pos',obj)
                                 | otherwise   = (pos',obj')

-- assumes that there is a rock at oldPos
-- if the rock doesn't move, return Nothing

newRockPos :: Mine -> Pos -> Maybe Pos
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

noLambdas :: Mine -> Bool
noLambdas = all (/= Lambda) . elems . grid

rockPos :: Mine -> [Pos]
rockPos = objPos Rock

objAt :: Mine -> Pos -> Obj
objAt mine pos = grid mine ! pos
