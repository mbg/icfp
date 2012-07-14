
module Trampolines where

import Core
import Data.Maybe (fromJust)
import Data.Array.IArray ((!))

removeTrampolines :: [Char] -> Obj -> Obj
removeTrampolines cs (Trampoline c') | c' `elem` cs = Empty
removeTrampolines _ obj = obj

-- given the letter for a target, find all trampoline letters that lead here

toThisTarget :: Char -> Mine -> [Char]
toThisTarget c = map fst . filter ((== c) . snd) . trampolines

-- given the position of a trampoline, find its corresponding target

jumpDest :: Mine -> Pos -> Pos
jumpDest mine pos = head (objPos (Target . fromJust . lookup c . trampolines $ mine) mine)
    where
    (Trampoline c) = grid mine ! pos