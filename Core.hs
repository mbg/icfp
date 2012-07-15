{-# OPTIONS_GHC -funbox-strict-fields #-}

module Core where

import Prelude hiding (Either(..))
import qualified Prelude as P
import Control.Applicative ((<$>))
import Data.Array.IArray
import Data.Ix
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Debug.Trace (trace)
import GHC.Arr (unsafeIndex, unsafeRangeSize)

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

isTrampoline :: Obj -> Bool
isTrampoline (Trampoline _) = True
isTrampoline _ = False

-- Progress: \x y where x = number of steps, y = number of lambdas
-- Final: \x where x = total score together

data Score = Progress !Int !Int | Final !Int

finishedScore :: Mine -> Maybe Int
finishedScore = score . finalScore
    where
    score (Final s) = Just s
    score _         = Nothing

data Mine = Mine 
    { grid             :: Array Pos Obj
    , flooding         :: FloodingState
    , beardData        :: BeardGrowth  
    , rockLocs         :: [Pos] 
    , beardLocs        :: [Pos]  
    , openLiftPos      :: [Pos]  
    , closedLiftPos    :: [Pos]  
    , trampolines      :: [(Char,Char)]
    , finalScore       :: Score }

incSteps  mn = let Progress s l = finalScore mn in mn { finalScore = Progress (s + 1) l }
incLambda mn = let Progress s l = finalScore mn in mn { finalScore = Progress s (l + 1) }

showMine :: Mine -> String
showMine mine = unlines (grid' ++ metaData)
    where
    grid' = reverse . splitAtEvery width . map toChar . elems . grid $ mine
    metaData = [ "Flooding: "    ++ show (flooding mine)
               , "Trampolines: " ++ show (trampolines mine)
               , "Beard: "       ++ show (beardData mine)]
    splitAtEvery :: Int -> [a] -> [[a]]
    splitAtEvery _ [] = []
    splitAtEvery n xs = let (x,xs') = splitAt n xs in x : splitAtEvery n xs'
    (width, _) = mineSize mine

mineSize :: Mine -> (Int, Int)
mineSize = unPos . snd .  bounds . grid

instance Show Mine where
   show = showMine

data Pos = Pos !Int !Int
    deriving (Eq, Ord, Show)
unPos (Pos x y) = (x, y)

objAt :: Mine -> Pos -> Obj
objAt mine pos = grid mine ! pos

mapObjs :: (Obj -> Obj) -> Mine -> Mine
mapObjs f mine = mine {grid = f <$> grid mine}

-- move an object from its old position to a new position and leave Empty behind
moveObj :: Mine -> Pos -> Pos -> Mine
moveObj mine old new = setObj (objAt mine old) (setObj Empty mine old) new

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

-- to get [(1,1), (2,1), (3,1), ...] order
instance Ix Pos where
    range       (Pos x1 y1, Pos x2 y2)           = [Pos x y | y <- range (y1,y2), x <- range (x1,x2)]
    unsafeIndex (Pos x1 y1, Pos x2 y2) (Pos x y) = unsafeIndex (y1,y2) y * unsafeRangeSize (x1,x2) + unsafeIndex (x1,x2) x
    inRange     (Pos x1 y1, Pos x2 y2) (Pos x y) = inRange (x1,x2) x && inRange (y1,y2) y
    {-# INLINE range #-}
    {-# INLINE unsafeIndex #-}
    {-# INLINE inRange #-}
    
data Cmd = Left
         | Right
         | Up
         | Down
         | Wait
         | Abort
         | Cut -- w/ Hutton's Razor 
         deriving (Eq, Ord, Show)

cmds :: [Cmd]
cmds = [Left, Right, Up, Down, Cut, Wait {-Abort-}] --, Wait, Abort, Cut]

type Path = [Cmd]

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

robotPos :: Mine -> Pos
robotPos = head . objPos Robot


-- these are bad, mkay?

objPos :: Obj -> Mine -> [Pos]
objPos obj = objsPos [obj]

objsPos :: [Obj] -> Mine -> [Pos]
objsPos objs = map fst . filter (\(pos, obj') -> obj' `elem` objs) . assocs . grid

---

setObj :: Obj -> Mine -> Pos -> Mine
setObj obj mine pos = mine{grid = array bounds' . map setObjCell . assocs $ (grid mine)}
    where bounds' = bounds (grid mine)
          setObjCell (pos',obj') | pos' == pos = (pos',obj)
                                 | otherwise   = (pos',obj')
