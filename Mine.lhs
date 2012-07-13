
> module Mine (
>    Obj(..),
>    Mine,
>    Pos,
>    toChar,
>    toObj,
>    showMap
> ) where

> data Obj = Robot 
>          | Wall
>          | Rock  
>          | Lambda
>          | ClosedLift
>          | OpenLift
>          | Earth 
>          | Empty
>          deriving (Eq, Ord, Enum)

> type Mine = [[Obj]]
> type Pos  = (Int, Int)

> toChar :: Obj -> Char
> toChar obj = ['R','#','*','\\','L','O','.',' '] !! (fromEnum obj)

> toObj :: Char -> Obj
> toObj 'R'  = Robot
> toObj '#'  = Wall
> toObj '*'  = Rock
> toObj '\\' = Lambda
> toObj 'L'  = ClosedLift
> toObj 'O'  = OpenLift
> toObj '.'  = Earth
> toObj ' '  = Empty

> showMap :: Mine -> String
> showMap = unlines . map (map toChar)

