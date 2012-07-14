module Core where

import Prelude hiding (Either(..))
import Data.Array.IArray (Array)
import Data.Ix
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Debug.Trace (trace)

data Obj = Robot
         | Wall
         | Rock
         | Lambda
         | ClosedLift
         | OpenLift
         | Earth
         | Empty
         deriving (Eq, Ord, Enum, Show)

data Mine = Mine 
    { grid     :: Array Pos Obj
    , flooding :: FloodingState }

newtype Pos = Pos {unPos :: (Int, Int)}
    deriving (Eq, Ord, Show)

data FloodingState = FloodingState
    { waterLevel         :: Int
    , floodingSpeed      :: Int
    , waterProofing      :: Int
    , stepsUntilNextRise :: Int
    , waterProofingLeft  :: Int }

-- in order to get [(1,1), (2,1), (3,1), ...] order
instance Ix Pos where
    range (Pos pos1, Pos pos2)                   = map (Pos . swap) (range (pos1, pos2))
    index (Pos (x1,y1), Pos (x2,y2)) (Pos (x,y)) = trace ("indexing: " ++ show (x,y)) (index ((y1,x1),(y2,x2)) (y,x))
    inRange (Pos pos1, Pos pos2) (Pos x)         = inRange (pos1, pos2) x

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
toChar = fromJust . flip lookup (map swap charObjs)

toObj :: Char -> Obj
toObj = fromJust . flip lookup charObjs

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
