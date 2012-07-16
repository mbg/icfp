{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE Rank2Types,
             TypeSynonymInstances,
             FlexibleInstances #-}
-- XXX: these are only for displaying in GHCi

module Core where

import Prelude hiding (Either(..))
import qualified Prelude as P
import Control.Applicative ((<$>))
import Control.Monad.ST
import Data.Function (on)
import Data.Ix
import Data.List (sort)
import Data.Maybe (fromJust, catMaybes)
import Data.Tuple (swap)
import qualified Data.Set as S
import Data.Vector (Vector, freeze, thaw, toList, (!))
import qualified Data.Vector.Mutable as MV
import Debug.Trace (trace)
import GHC.Arr (unsafeIndex, unsafeRangeSize)

data Pos = Pos !Int !Int
    deriving (Eq, Ord, Show)
unPos (Pos x y) = (x, y)

data Obj = Robot
         | Wall
         | Rock
         | Lambda
         | ClosedLift
         | OpenLift
         | Earth
         | Empty
         | Trampoline !Char
         | Target     !Char
         | Beard  
         | Razor  
         | HOLambda 
         deriving (Eq, Ord, Show)

data Cmd = Left
         | Right
         | Up
         | Down
         | Wait
         | Abort
         | Cut -- w/ Hutton's Razor 
         deriving (Eq, Ord, Show)

cmds :: [Cmd]
cmds = [Left, Right, Up, Down, Wait {-, Cut, Abort-}]

data FloodingState = FloodingState
    { waterLevel         :: !Int
    , floodingSpeed      :: !Int
    , waterProofing      :: !Int
    , stepsSinceLastRise :: !Int
    , waterProofingLeft  :: !Int }
    deriving Show

data BeardGrowth = BeardGrowth 
    { numberRazors       :: !Int 
    , beardGrowthRate    :: !Int 
    , stepsToGrowth      :: !Int } 
    deriving Show

-- Progress: \x y where x = number of steps, y = number of lambdas
-- Final: \x where x = total score together

data Score = Progress !Int !Int | Final !Int

-- to get [(1,1), (2,1), (3,1), ...] order
instance Ix Pos where
    range       (Pos x1 y1, Pos x2 y2)           = [Pos x y | y <- range (y1,y2), x <- range (x1,x2)]
    unsafeIndex (Pos x1 y1, Pos x2 y2) (Pos x y) = unsafeIndex (y1,y2) y * unsafeRangeSize (x1,x2) + unsafeIndex (x1,x2) x
    inRange     (Pos x1 y1, Pos x2 y2) (Pos x y) = inRange (x1,x2) x && inRange (y1,y2) y
    {-# INLINE range #-}
    {-# INLINE unsafeIndex #-}
    {-# INLINE inRange #-}

isTrampoline :: Obj -> Bool
isTrampoline (Trampoline _) = True
isTrampoline _ = False

type Path = [Cmd]

type MutableGrid s = MV.MVector s Obj
type Grid = Vector Obj

type Mine = (Grid, MineInfo)
type MutableMine s = (MutableGrid s, MineInfo)

-- maybe make these sets instead of lists, maybe remove rockLocs
data MineInfo = MineInfo
    { bounds           :: (Pos, Pos)
    , flooding         :: FloodingState
    , beardData        :: BeardGrowth  
    , lambdaLocs       :: S.Set Pos
    , rockLocs         :: S.Set Pos
    , hoLocs           :: S.Set Pos
    , beardLocs        :: S.Set Pos
    , robotLoc         :: !Pos
    , liftLoc          :: !Pos
    , trampolines      :: [(Char,Char)]
    , finalScore       :: Score }


addLambda, addRock, addBeard, addHO :: MineInfo -> Pos -> MineInfo
addLambda info pos = info{lambdaLocs = pos `S.insert` lambdaLocs info}
addRock   info pos = info{rockLocs   = pos `S.insert` rockLocs   info}
addBeard  info pos = info{beardLocs  = pos `S.insert` beardLocs  info}
addHO     info pos = info{hoLocs     = pos `S.insert` hoLocs     info}

remLambda, remRock, remBeard, remHO :: MineInfo -> Pos -> MineInfo
remLambda info pos = info{lambdaLocs = pos `S.delete` lambdaLocs info}
remRock   info pos = info{rockLocs   = pos `S.delete` rockLocs   info}
remBeard  info pos = info{beardLocs  = pos `S.delete` beardLocs  info} 
remHO     info pos = info{hoLocs     = pos `S.delete` hoLocs     info}

freezeMine :: (forall s. ST s (MutableMine s)) -> Mine
freezeMine stMine = runST $ do
    (mVec, info) <- stMine
    vec          <- freeze mVec
    return $! (vec, info)

freezeMaybeMine :: (forall s. ST s (Maybe (MutableMine s))) -> Maybe Mine
freezeMaybeMine stMine = runST  $ do
    maybeMine <- stMine
    case maybeMine of
        Nothing           -> return Nothing
        Just (mVec, info) -> do
            vec <- freeze mVec
            return $! Just (vec, info)

thawMine :: Mine -> (forall s. ST s (MutableMine s))
thawMine (grid, info) = do
    mGrid <- thaw grid
    return (mGrid, info)

incSteps, incLambda :: (a, MineInfo) -> (a, MineInfo)
incSteps  (grid, info) = let Progress s l = finalScore info in (grid, info{finalScore = Progress (s + 1) l})
incLambda (grid, info) = let Progress s l = finalScore info in (grid, info{finalScore = Progress s (l + 1)})

finishedScore :: (a, MineInfo) -> Maybe Int
finishedScore (_, info) = score (finalScore info)
    where
    score (Final s) = Just s
    score _         = Nothing

-- {-# WARNING unsafeMapObj "This is only for use for types not indexed in MineInfo" #-}
-- DO NOT USE THIS UNLESS YOU'RE A TRAMPOLINE
unsafeMapObj :: forall s. (Obj -> Obj) -> MutableMine s -> ST s (MutableMine s)
unsafeMapObj f (mGrid, info) = do
    sequence_ $ [MV.read mGrid ix >>= MV.write mGrid ix . f | ix <- map (unsafeIndex (bounds info)) (range (bounds info))]
    return (mGrid, info)

-- please don't use this often. Really.
findObjs :: forall s. (Obj -> Bool) -> MutableMine s -> ST s [Pos]
findObjs f (mGrid, info) = catMaybes <$> sequence [ (\obj -> if f obj then Just posIx else Nothing) <$> MV.read mGrid ix
                                                  | posIx <- (range (bounds info))
                                                  , let ix = unsafeIndex (bounds info) posIx]

showMine :: Mine -> String
showMine (grid, info) = unlines (grid' ++ metaData)
    where
    grid' = reverse . splitAtEvery width . map toChar . toList $ grid
    metaData = [ "Flooding: "    ++ show (flooding info)
               , "Trampolines: " ++ show (trampolines info)
               ] --, "Beard: "       ++ show (beardData info)]
    splitAtEvery :: Int -> [a] -> [[a]]
    splitAtEvery _ [] = []
    splitAtEvery n xs = let (x,xs') = splitAt n xs in x : splitAtEvery n xs'
    (width, _) = mineSize (grid, info)

mineSize :: (a, MineInfo) -> (Int, Int)
mineSize (_, info) = let (Pos lx ly, Pos ux uy) = bounds info in (ux - lx + 1, uy - ly + 1)

toChar :: Obj -> Char
toChar Robot          = 'R'
toChar Wall           = '#'
toChar Rock           = '*'
toChar Lambda         = '\\'
toChar ClosedLift     = 'L'
toChar OpenLift       = 'O'
toChar Earth          = '.'
toChar Empty          = ' '
toChar Beard          = 'W'
toChar Razor          = '!'
toChar HOLambda       = '@'
toChar (Trampoline c) = c
toChar (Target     c) = c

toObj :: Char -> Obj
toObj 'R'  = Robot
toObj '#'  = Wall
toObj '*'  = Rock
toObj '\\' = Lambda
toObj 'L'  = ClosedLift
toObj 'O'  = OpenLift
toObj '.'  = Earth
toObj ' '  = Empty
toObj 'W'  = Beard
toObj '!'  = Razor
toObj '@'  = HOLambda
toObj char | char `elem` ['A'..'I'] = Trampoline char
           | char `elem` ['1'..'9'] = Target char

objAt :: Mine -> Pos -> Obj
objAt (grid, info) pos = grid ! unsafeIndex (bounds info) pos

objAtM :: forall s. MutableMine s -> Pos -> ST s Obj
objAtM (mGrid, info) pos = MV.read mGrid (unsafeIndex (bounds info) pos)

{-# WARNING setObj "setObj does not update the mineInfo, are you sure you don't want to use moveObj instead?" #-}
setObj :: forall s. MutableMine s -> Pos -> Obj -> ST s ()
setObj (mGrid, info) pos obj = MV.write mGrid (unsafeIndex (bounds info) pos) obj

addObjInfo :: MineInfo -> Pos -> Obj -> MineInfo
addObjInfo info pos Lambda   = addLambda info pos
addObjInfo info pos Rock     = addRock   info pos
addObjInfo info pos Beard    = addBeard  info pos
addObjInfo info pos HOLambda = addHO     info pos
addObjInfo info _   _        = info

remObjInfo :: MineInfo -> Pos -> Obj -> MineInfo
remObjInfo info pos Lambda   = remLambda info pos
remObjInfo info pos Rock     = remRock   info pos
remObjInfo info pos Beard    = remBeard  info pos
remObjInfo info pos HOLambda = remHO     info pos
remObjInfo info _   _        = info

moveObjInfo :: MineInfo -> Pos -> Pos -> Obj -> MineInfo
moveObjInfo info _   new Robot = info{robotLoc = new}
moveObjInfo info old new obj   = addObjInfo (remObjInfo info old obj) new obj

-- move an object from its old position to a new position and leave Empty behind
moveObj :: forall s. MutableMine s -> Pos -> Pos -> ST s (MutableMine s)
moveObj (mGrid, info) old new = do
    obj <- objAtM (mGrid, info) old
    setObj (mGrid, info) old Empty
    setObj (mGrid, info) new obj
    return (mGrid, moveObjInfo info old new obj)

showCmd :: Cmd -> Char
showCmd Left  = 'L'
showCmd Right = 'R'
showCmd Up    = 'U'
showCmd Down  = 'D'
showCmd Wait  = 'W'
showCmd Abort = 'A'
showCmd Cut   = 'S'

showPath :: Path -> String
showPath = map showCmd

move :: Pos -> Cmd -> Pos
move (Pos x y) Left  = Pos (x - 1)  y
move (Pos x y) Right = Pos (x + 1)  y
move (Pos x y) Up    = Pos  x      (y + 1)
move (Pos x y) Down  = Pos  x      (y - 1)
move (Pos x y) Wait  = Pos  x       y
move _         Abort = error "~gmh for prime minister"

isRocklike :: Obj -> Bool
isRocklike Rock     = True
isRocklike HOLambda = True
isRocklike _        = False

robotPos :: (a, MineInfo) -> Pos
robotPos (_, info) = robotLoc info

lambdaPos :: (a, MineInfo) -> [Pos]
lambdaPos (_, info) = S.toList (lambdaLocs info)

hoLambdaPos :: (a, MineInfo) -> [Pos]
hoLambdaPos (_, info) = S.toList (hoLocs info)

rocklikePos :: (a, MineInfo) -> [Pos]
rocklikePos (_, info) = S.toList (rockLocs info) ++ S.toList (hoLocs info)

liftPos :: (a, MineInfo) -> Pos
liftPos (_, info) = liftLoc info

-- these are bad, mkay?
{-
objPos :: Obj -> Mine -> [Pos]
objPos obj = objsPos [obj]

objsPos :: [Obj] -> Mine -> [Pos]
objsPos objs = map fst . filter (\(pos, obj') -> obj' `elem` objs) . assocs . grid
-}

initParseObjs :: Grid -> (Pos, Pos) -> (S.Set Pos, S.Set Pos, S.Set Pos, S.Set Pos, Pos, Pos)
initParseObjs grid bounds' =
    ( S.fromList (map snd lambdas)
    , S.fromList (map snd rocks)
    , S.fromList (map snd beards)
    , S.fromList (map snd hos)
    , robot
    , lift)
    where
    interesting (obj, _) = obj `elem` [Lambda, Rock, Beard, OpenLift, ClosedLift, HOLambda, Robot]
    ((Robot, robot):relevant) = sort $ filter interesting (zip (toList grid) (range bounds'))
    (rocks, objs) = span ((== Rock) . fst) relevant
    (lambdas, objs') = span ((== Lambda) . fst) objs
    (lift, objs''') = case objs' of
        (OpenLift, pos):objs''   -> (pos, objs'')
        (ClosedLift, pos):objs'' -> (pos, objs'')
    (beards, objs'''') = span ((== Beard) . fst) objs'''
    (hos, _) = span ((== HOLambda) . fst) objs''''
    