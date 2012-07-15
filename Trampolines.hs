
module Trampolines where

import Core
import Data.Maybe (fromJust)
import Data.Array.IArray ((!))

-- given the position of a trampoline, jump the robot
jump :: Mine -> Pos -> Mine
jump mine trampoline = moveObj mine' loc jumpDest'
    where
    loc         = robotPos mine
    jumpDest'   = jumpDest mine trampoline
    Target c      = objAt mine jumpDest'
    trampoChars = toThisTarget c mine
    mine'       = removeTrampolines trampoChars mine

removeTrampolines :: [Char] -> Mine -> Mine
removeTrampolines cs = mapObjs (removeTrampoline cs)

removeTrampoline :: [Char] -> Obj -> Obj
removeTrampoline cs (Trampoline c') | c' `elem` cs = Empty
removeTrampoline _ obj = obj

-- given the letter for a target, find all trampoline letters that lead here

toThisTarget :: Char -> Mine -> [Char]
toThisTarget c = map fst . filter ((== c) . snd) . trampolines

-- given the position of a trampoline, find its corresponding target

jumpDest :: Mine -> Pos -> Pos
jumpDest mine pos = head (objPos (Target . fromJust . lookup c . trampolines $ mine) mine)
    where
    (Trampoline c) = grid mine ! pos