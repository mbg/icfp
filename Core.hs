module Core where

import Prelude hiding (Either(..))
import Data.Array.IArray (Array)
import Data.Ix
import Data.Tuple (swap)

data Obj = Robot 
         | Wall
         | Rock  
         | Lambda
         | ClosedLift
         | OpenLift
         | Earth 
         | Empty
         deriving (Eq, Ord, Enum)

type Mine = Array Pos Obj
newtype Pos = Pos {unPos :: (Int, Int)}
    deriving (Eq, Ord, Show)

-- in order to get [(1,1), (2,1), (3,1), ...] order
instance Ix Pos where
    range (Pos pos1, Pos pos2)                   = map (Pos . swap) (range (pos1, pos2))
    index (Pos (x1,y1), Pos (x2,y2)) (Pos (x,y)) = index ((y1,x1),(y2,x2)) (y,x)
    inRange (Pos pos1, Pos pos2) (Pos x)         = inRange (pos1, pos2) x

data Cmd = Left
         | Right
         | Up
         | Down
         | Wait
         | Abort
         deriving (Eq, Ord)
    
type Path = [Cmd]

toChar :: Obj -> Char
toChar obj = ['R','#','*','\\','L','O','.',' '] !! (fromEnum obj)

toObj :: Char -> Obj
toObj 'R'  = Robot
toObj '#'  = Wall
toObj '*'  = Rock
toObj '\\' = Lambda
toObj 'L'  = ClosedLift
toObj 'O'  = OpenLift
toObj '.'  = Earth
toObj ' '  = Empty

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