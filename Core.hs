module Core where

import Prelude hiding (Either(..))
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
         | Trampoline Char
         | Target Char
         deriving (Eq, Ord, Show)

isTrampoline :: Obj -> Bool
isTrampoline (Trampoline _) = True
isTrampoline _ = False

data Mine = Mine 
    { grid     :: Array Pos Obj
    , flooding :: FloodingState
    , trampolines :: [(Char,Char)]}

showMine :: Mine -> String
showMine mine = unlines (grid' ++ metaData)
    where
    grid' = reverse . splitAtEvery width . map toChar . elems . grid $ mine
    metaData = ["Flooding: " ++ show (flooding mine), "Trampolines: " ++ show (trampolines mine)]
    splitAtEvery :: Int -> [a] -> [[a]]
    splitAtEvery _ [] = []
    splitAtEvery n xs = let (x,xs') = splitAt n xs in x : splitAtEvery n xs'
    (width, _) = mineSize mine

mineSize :: Mine -> (Int, Int)
mineSize = unPos . snd .  bounds . grid

instance Show Mine where
   show = showMine

newtype Pos = Pos {unPos :: (Int, Int)}
    deriving (Eq, Ord, Show)

data FloodingState = FloodingState
    { waterLevel         :: Int
    , floodingSpeed      :: Int
    , waterProofing      :: Int
    , stepsSinceLastRise :: Int
    , waterProofingLeft  :: Int }
    deriving Show

-- to get [(1,1), (2,1), (3,1), ...] order
instance Ix Pos where
    range       (Pos (x1,y1), Pos (x2,y2))             = [Pos (x,y) | y <- range (y1,y2), x <- range (x1,x2)]
    unsafeIndex (Pos (x1,y1), Pos (x2,y2)) (Pos (x,y)) = unsafeIndex (y1,y2) y * unsafeRangeSize (x1,x2) + unsafeIndex (x1,x2) x
    inRange     (Pos (x1,y1), Pos (x2,y2)) (Pos (x,y)) = inRange (x1,x2) x && inRange (y1,y2) y
data Cmd = Left
         | Right
         | Up
         | Down
         | Wait
         | Abort
         deriving (Eq, Ord, Show)

dirs :: [Cmd]
dirs = [Left, Right, Up, Down]

type Path = [Cmd]

toChar :: Obj -> Char
toChar (Trampoline char) = char
toChar (Target char) = char
toChar obj = fromJust . flip lookup (map swap charObjs) $ obj

toObj :: Char -> Obj
toObj char | char `elem` ['A'..'I'] = Trampoline char
           | char `elem` ['1'..'9'] = Target char
           | otherwise = fromJust . flip lookup charObjs $ char

charObjs :: [(Char, Obj)]
charObjs =
    [('R' , Robot)
    ,('#' , Wall)
    ,('*' , Rock)
    ,('\\', Lambda)
    ,('L' , ClosedLift)
    ,('O' , OpenLift)
    ,('.' , Earth)
    ,(' ' , Empty)]

showCmd :: Cmd -> Char
showCmd Left  = 'L'
showCmd Right = 'R'
showCmd Up    = 'U'
showCmd Down  = 'D'
showCmd Wait  = 'W'
showCmd Abort = 'A'

showPath :: Path -> String
showPath = map showCmd

move :: Pos -> Cmd -> Pos
move (Pos (x, y)) Left  = Pos (x - 1, y)
move (Pos (x, y)) Right = Pos (x + 1, y)
move (Pos (x, y)) Up    = Pos (x, y + 1)
move (Pos (x, y)) Down  = Pos (x, y - 1)
move (Pos (x, y)) Wait  = Pos (x, y)
move _            Abort = error "~gmh for prime minister"

robotPos :: Mine -> Pos
robotPos = head . objPos Robot

objPos :: Obj -> Mine -> [Pos]
objPos obj = map fst . filter (\(pos, obj') -> obj == obj') . assocs . grid
