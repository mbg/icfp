{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module Mine where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.ST
import Data.Either (partitionEithers)
import Data.Ix
import Data.List (stripPrefix)
import Data.Maybe (isJust, catMaybes, fromMaybe, fromJust)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Debug.Trace
import Prelude hiding (Left, Right)
import qualified Prelude as P
import Debug.Trace (trace)

import Core
import Flooding
import Trampolines
--import Growths XXX: ignoring growths for now

readMine :: String -> Mine
readMine str = (grid, info)
    where
    (lambdas, rocks, beards, hos, robot, lift) = initParseObjs grid bounds'
    grid = V.fromList (concatMap (pad maxX . map toObj) (reverse rows))
    info = MineInfo
        { bounds        = bounds'
        , flooding      = parseFlooding metaData
        , beardData     = parseBeard metaData
        , lambdaLocs    = lambdas
        , rockLocs      = rocks
        , beardLocs     = beards
        , hoLocs        = hos
        , liftLoc       = lift
        , robotLoc      = robot
        , trampolines   = parseTrampolines metaData
        , finalScore    = Progress 0 0}
    bounds' = (Pos 1 1, Pos maxX maxY)
    (rows, metaData) = break null (lines str)
    maxY = length rows
    maxX = maximum (map length rows)
    pad :: Int -> [Obj] -> [Obj]
    pad n xs = take n (xs ++ repeat Empty)

parseBeard :: [String] -> BeardGrowth
parseBeard css = fromMaybe defaultBeard (BeardGrowth <$> numberRazors' <*> beardGrowthRate' <*> ((subtract 1) <$> beardGrowthRate'))
    where numberRazors'    = getOpt "Razors " css
          beardGrowthRate' = getOpt "Growth " css
          defaultBeard     = undefined --makeBeardGrowth 0 25

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

hasOpenLift :: (a, MineInfo) -> Bool
hasOpenLift (_, info) = S.null (lambdaLocs info) && S.null (hoLocs info)

findOpenLift :: (a, MineInfo) -> Pos
findOpenLift (_, info) = liftLoc info


-- if the move is possible, return Just result
-- if the move is not possible, return Nothing
robotCmd :: forall s. MutableMine s -> Cmd -> ST s (Maybe (MutableMine s))
robotCmd mine Abort             = return $! Just . failure $ mine
{-
robotCmd mine Cut
    | numberRazors (beardData mine) > 0 &&
      beardsNearby mine > 0     = return . Just . applyRazor $ mine
    | otherwise                 = return Nothing -}
robotCmd mine Wait              = return $! Just mine
robotCmd mine cmd = do
    let loc   = robotPos mine
        dest  = move loc cmd
    obj <- objAtM mine dest
    let moved = incSteps <$> moveObj mine loc dest
        remLambda (mMine, info) = (mMine, remObjInfo info dest Lambda)
    case obj of
        ClosedLift | hasOpenLift mine   -> return . Just . victory   =<< moved
        Lambda                          -> return . Just . incLambda . remLambda =<< moved
        _   | obj `elem` [Empty, Earth] -> return . Just             =<< moved
            | isRocklike obj &&
              cmd `elem` [Left, Right]  -> pushObj mine cmd
            | isTrampoline obj          -> Just . incSteps <$> jump mine dest -- check that this updates the robotPos field
            | otherwise                 -> return Nothing

-- Only call with a cmd of Left or Right and with a pushable object in front of it, this is not checked
pushObj :: forall s. MutableMine s -> Cmd -> ST s (Maybe (MutableMine s))
pushObj mine cmd
    | inBounds' = do
        obj <- objAtM mine dest2
        case obj of
            Empty -> do
                mine'  <- moveObj mine dest dest2
                mine'' <- moveObj mine' loc dest
                return $! Just . incSteps $ mine''
            _ -> return Nothing
    | otherwise = return Nothing
    where loc   = robotPos mine
          dest  = move loc cmd
          dest2 = move dest cmd
          inBounds' = inBounds mine dest2

-- finalise a map we've aborted
failure :: forall s. MutableMine s -> MutableMine s
failure (mGrid, info) = let Progress s l = finalScore info in (mGrid, info{ finalScore = Final $! (l * 50) - s })

-- finalise a map we've moved onto the open lift of
victory :: forall s. MutableMine s -> MutableMine s
victory (mGrid, info) = let Progress s l = finalScore info in (mGrid, info{ finalScore = Final $! (l * 75) - s })

inBounds :: (a, MineInfo) -> Pos -> Bool
inBounds (_, info) = inRange (bounds info)

nextPossibleStates :: Mine -> [Mine]
nextPossibleStates mine = catMaybes . map (flip updateMine mine) $ cmds

-- thaw the mine, update with updateEmb and robotCmd, freeze it
updateMine :: Cmd -> Mine -> Maybe Mine
updateMine cmd mine = freezeMaybeMine (updateEnv =<< flip robotCmd cmd =<< thawMine mine)

-- if we return Nothing, then the robot would have died
-- let's not bother updating the closed lift to an open one - it's implied by the number of lambdas remaining
updateEnv :: forall s. (Maybe (MutableMine s)) -> ST s (Maybe (MutableMine s))
updateEnv (Just mine) = do -- updateBeards
    mMine <- moveRocks mine
    return (updateWater =<< mMine)
updateEnv Nothing = return Nothing

-- moves the rocks in the mine and also returns if it actually moved any
-- if we have a rock above a robot's head that wasn't there in the original mine, return Nothing
-- TODO: change falled hoLambdas into Lambdas
moveRocks :: forall s. MutableMine s -> ST s (Maybe (MutableMine s))
moveRocks mine = do
    oldNewPairs  <- mapM getPair (rocklikePos mine)
    if not (any (isJust . snd) oldNewPairs)
        then return $! Just mine
        else do
            oldRockAbove <- isRocklike <$> objAtM mine above
            mine'        <- foldM maybeMove mine oldNewPairs
            newRockAbove <- isRocklike <$> objAtM mine' above
            if newRockAbove && not oldRockAbove
                then return Nothing
                else return $! Just mine'

    where
    getPair pos = newRockPos mine pos >>= \new -> return (pos,new)
    above = move (robotPos mine) Up
    maybeMove mine (old, Just new) = moveObj mine old new
    maybeMove mine _               = return mine

-- assumes that there is a rock at oldPos
-- if the rock doesn't move, return Nothing
newRockPos :: forall s. MutableMine s -> Pos -> ST s (Maybe Pos)
newRockPos mine pos = do
    objDown <- objAtM mine down
    case objDown of
        Lambda -> tryFallRight
        Empty  -> return (Just down)
        _ | isRocklike objDown -> do
                mResult <- tryFallRight
                if isJust mResult then return mResult else tryFallLeft
          | otherwise -> return Nothing
    where down      = move pos Down
          downLeft  = move (move pos Down) Left
          downRight = move (move pos Down) Right
          left      = move pos Left
          right     = move pos Right
          tryFallRight = do
            objRight  <- objAtM mine right
            objDRight <- objAtM mine downRight
            case (objRight, objDRight) of
                (Empty, Empty) -> return (Just downRight)
                _              -> return Nothing
          tryFallLeft =  do
            objLeft <- objAtM mine left
            objDLeft <- objAtM mine downLeft
            case (objLeft, objDLeft) of
                (Empty, Empty) -> return (Just downLeft)
                _              -> return Nothing
