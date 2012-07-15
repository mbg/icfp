{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module Mine where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Array.IArray
import Data.List (stripPrefix)
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
                    , beardData = parseBeard metaData             
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

parseBeard :: [String] -> BeardGrowth
parseBeard css = fromMaybe defaultBeard (BeardGrowth <$> numberRazors' <*> beardGrowthRate' <*> ((subtract 1) <$> beardGrowthRate'))
    where numberRazors'    = getOpt "Razors " css
          beardGrowthRate' = getOpt "Growth " css
          defaultBeard     = BeardGrowth 0 25 24

parseFlooding :: [String] -> FloodingState
parseFlooding css = fromMaybe defaultFlooding (makeFloodingState <$> level' <*> flooding' <*> waterproof')
    where level'      = getOpt "Water "      css
          flooding'   = getOpt "Flooding "   css
          waterproof' = getOpt "Waterproof " css

getOpt :: Read a => String -> [String] -> Maybe a
getOpt xs xss = read <$> msum (map (stripPrefix xs) xss)

parseTrampolines :: [String] -> [(Char, Char)]
parseTrampolines = catMaybes . map parseTrampoline

parseTrampoline :: String -> Maybe (Char, Char)
parseTrampoline str = do
    (tramp:str') <- stripPrefix "Trampoline " str
    (target:_  ) <- stripPrefix " targets "   str'
    return (tramp, target)


-- does NOT include the falling rocks, the main
-- function will deal with this

robotCmd :: Mine -> Cmd -> Maybe Mine
robotCmd mine Abort            = Just (failure mine)
robotCmd mine Cut
    | razorsLeft > 0 &&
      adjacentBeards > 0        = Just (applyRazor mine)
    where
    razorsLeft = numberRazors (beardData mine)
    adjacentBeards = error "adjacentBeards not implemented yet"
robotCmd mine Wait              = Just mine
robotCmd mine cmd
    | obj == OpenLift           = Just (victory (incSteps moved))
    | obj `elem` [Empty, Earth] = Just moved
    | isRocklike obj &&
      cmd `elem` [Left, Right]  = pushObj mine cmd
    | isTrampoline obj          = Just (incSteps (jump mine dest))
    | obj == Lambda             = Just (incLambda moved)
    where
    loc   = robotPos mine
    dest  = move loc cmd
    obj   = objAt mine dest
    moved = incSteps (moveObj mine loc dest)
robotCmd _    _                 = Nothing

-- Only call with a cmd of Left or Right and with a pushable object in front of it, this is not checked
pushObj :: Mine -> Cmd -> Maybe Mine
pushObj mine cmd
    | inBounds mine dest2 &&
      objAt mine dest2 == Empty = Just (incSteps (moveObj mine' loc dest))
    | otherwise                 = Nothing
    where loc   = robotPos mine
          dest  = move loc cmd
          dest2 = move dest cmd
          mine' = moveObj mine dest dest2

-- finalise a map we've aborted
failure :: Mine -> Mine
failure = error "failure not implemented yet"

-- finalise a map we've moved onto the open lift of
victory :: Mine -> Mine
victory = error "victory not implemented yet"
          
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

inBounds :: Mine -> Pos -> Bool
inBounds mine = inRange (bounds (grid mine))

nextPossibleStates :: Mine -> [Mine]
nextPossibleStates mine = catMaybes (map (flip updateMine mine) dirs)

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

-- if we return Nothing, then the robot would have died
updateEnv :: Mine -> Maybe Mine
updateEnv mine = openLiftH . updateBeards <$> (updateWater =<< moveRocks mine)

updateMine :: Cmd -> Mine -> Maybe Mine
updateMine cmd = updateEnv <=< flip robotCmd cmd


openLiftH :: Mine -> Mine
openLiftH m | noLambdas m = mapObjs openLift m
            | otherwise   = m

openLift :: Obj -> Obj
openLift ClosedLift = OpenLift
openLift obj        = obj

-- moves the rocks in the mine and also returns if it actually moved any
-- if we have a rock above a robot's head that wasn't there in the original mine, return Nothing
moveRocks :: Mine -> Maybe Mine
moveRocks mine | not (any (isJust . snd) oldNewPairs) = Just mine -- no rocks were moved
               | wasCrushed                           = Nothing
               | otherwise                            = Just mine'
    where
    oldNewPairs :: [(Pos, Maybe Pos)]
    oldNewPairs  = map (\pos -> (pos, newRockPos mine pos)) (rockPos mine)
    mine'        = foldl maybeMove mine oldNewPairs

    maybeMove :: Mine -> (Pos, Maybe Pos) -> Mine
    maybeMove mine (old, Just new) = moveObj mine old new
    maybeMove mine _               = mine

    wasCrushed :: Bool
    -- is there a rock above the robot's head in mine' that wasn't there in mine?
    wasCrushed = objAt mine' (move (robotPos mine) Up) == Rock
              && objAt mine  (move (robotPos mine) Up) /= Rock


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
