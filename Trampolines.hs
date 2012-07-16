{-# LANGUAGE Rank2Types #-}
module Trampolines where

import Core

import Control.Applicative ((<$>))
import Control.Monad.ST
import Data.Maybe (fromJust)

-- given the position of a trampoline, jump the robot
jump :: forall s. MutableMine s -> Pos -> ST s (MutableMine s)
jump mine trampoline = do
    let loc = robotPos mine
    jumpDest' <- jumpDest mine trampoline
    Target c  <- objAtM mine jumpDest'
    let trampoChars = toThisTarget c mine
    mine'     <- removeTrampolines trampoChars mine
    moveObj mine loc jumpDest'

-- this is quite expensive, good thing we don't do it often
removeTrampolines :: forall s. [Char] -> MutableMine s -> ST s (MutableMine s)
removeTrampolines cs mMine = unsafeMapObj (removeTrampoline cs) mMine

removeTrampoline :: [Char] -> Obj -> Obj
removeTrampoline cs (Trampoline c') | c' `elem` cs = Empty
removeTrampoline _ obj = obj

-- given the letter for a target, find all trampoline letters that lead here

toThisTarget :: Char -> (a, MineInfo) -> [Char]
toThisTarget c (_, info) = map fst . filter ((== c) . snd) . trampolines $ info

-- given the position of a trampoline, find its corresponding target
-- slow, but not called very often
jumpDest :: forall s. MutableMine s -> Pos -> ST s Pos
jumpDest mine pos = do 
    Trampoline c <- objAtM mine pos
    head <$> findObjs (== (Target . fromJust . lookup c . trampolines . snd $ mine)) mine
