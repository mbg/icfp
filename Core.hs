module Core (Obj(..), Mine(..), Cmd(..), Path, Pos, showCmd, showPath, move) where

import Prelude hiding (Either(..))

data Obj = Robot 
         | Wall
         | Rock  
         | Lambda
         | ClosedLift
         | OpenLift
         | Earth 
         | Empty
         deriving (Eq, Ord, Enum)

type Mine = [[Obj]]
type Pos  = (Int, Int)

data Cmd = Left
         | Right
         | Up
         | Down
         | Wait
         | Abort
         deriving (Eq, Ord)
    
type Path = [Cmd]

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
move (x, y) Left  = (x - 1, y)
move (x, y) Right = (x + 1, y)
move (x, y) Up    = (x, y + 1)
move (x, y) Down  = (x, y - 1)
move (x, y) Wait  = (x, y)
move (x, y) Abort = error "~gmh for prime minister"