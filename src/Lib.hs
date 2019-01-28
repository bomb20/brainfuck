{-
 - File              : Lib.hs
 - Author            : Vincent Truchseß <redtux@posteo.net>
 - Date              : 25.01.2019
 - Last Modified Date: 28.01.2019
 - Last Modified By  : Vincent Truchseß <redtux@posteo.net>
 -}
{-
 - src/Lib.hs
 - Copyright (c) 2019 Vincent Truchseß <redtux@posteo.net>
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}
module Lib where

import Data.Char
import Data.List

-- This Section concerns with the data-types we'll need to represent the
-- overall state of the brainfuck-interpreter.
-- The Tape consists of two infinite lists initialized with zeroes representing
-- the right and left side of the tape. In addition to that we have the
-- `current` element between those two, representing the head-position.
-- The following Definitions are ment to be used inside this library.
-- | The `Tape` type represents the infinite Tape of hte Brainfuck-Machine
data Tape = Tape
  { left :: [Int]
  , cur :: Int
  , right :: [Int]
  } deriving (Eq)

-- The show function prints out part of the tape and the programm, leaving blank
-- lines between outputs for in and output.
instance Show Tape where
  show (Tape left cur right) =
    unwords (map show (reverse (take 10 left))) ++
    "|" ++ show cur ++ "|" ++ unwords (map show (take 10 right))

-- Those functions return a tape with the current element being
-- incremented or decremented
incTape :: Tape -> Tape
incTape t@Tape {cur = c} = t {cur = (c + 1) `mod` 256}

decTape :: Tape -> Tape
decTape t@Tape {cur = c} = t {cur = (c - 1) `mod` 256}

moveRight :: Tape -> Tape
moveRight (Tape l c (r:rs)) = Tape (c : l) r rs

moveLeft :: Tape -> Tape
moveLeft (Tape (l:ls) c rs) = Tape ls l (c : rs)

-- The programm type holds a tuple of two Strings. The left one contains the
-- commands already executed, while the right one holds the ones still to come.
data Programm = Prog
  { prev :: String
  , cmd :: Char
  , next :: String
  }

-- The show implementation prints a section of the programm markingthe current
-- comand with | symbols.
instance Show Programm where
  show p@Prog {next = []} = show p {next = " "}
  show (Prog prev cmd next) =
    reverse (take 10 prev) ++ "|" ++ [cmd] ++ "|" ++ take 10 next

step :: State -> State
step s@State {prog = p} = s {prog = st p}
  where
    st (Prog prev c []) = Prog (c : prev) '\0' []
    st (Prog prev c (n:ns)) = Prog (c : prev) n ns

-- | The General machine State.
data State = State
  { tape :: Tape
  , prog :: Programm
  , stack :: [Programm]
  , runFkt :: State -> IO ()
  }

instance Show State where
  show State {tape = t, prog = p} = "\n" ++ show t ++ "  " ++ show p ++ "\n"

getCmd :: State -> Char
getCmd s@State {prog = p} = cmd p

-- | Performs an increment-operation on the current element of the tape
increment :: State -> State
increment s@State {tape = t} = step $ s {tape = incTape t}

-- | Performs a decrement operation on the current element of the tape
decrement :: State -> State
decrement s@State {tape = t} = step $ s {tape = decTape t}

-- | Moves the head tn the tape one cell to the right
goRight :: State -> State
goRight s@State {tape = t} = step $ s {tape = moveRight t}

-- | Moves the head on the tape one cell to the left
goLeft :: State -> State
goLeft s@State {tape = t} = step $ s {tape = moveLeft t}

-- | This function handles the beginning of loops
loopBegin :: State -> State
loopBegin s@State {tape = t, prog = pr@(Prog p c (n:ns)), stack = st} =
  case cur t of
    0 -> s {prog = go 1 (Prog (c : p) n ns)}
      where go 0 p = p
            go n (Prog p ']' []) = Prog (']' : p) '\0' []
            go n (Prog p '[' (x:xs)) = go (n + 1) (Prog ('[' : p) x xs)
            go n (Prog p ']' (x:xs)) = go (n - 1) (Prog (']' : p) x xs)
    _ -> s {stack = pr : st, prog = Prog (c : p) n ns}

-- | This function handles the end of loops
loopEnd :: State -> State
loopEnd s@State {stack = (x:xs), tape = t} =
  case cur t of
    0 -> step $ s {stack = xs}
    _ -> step $ s {prog = x}

-- | get the current element from the tape as char
getCur :: State -> Char
getCur State {tape = t} = chr $ cur t

-- | set the current element of the tape and go to next command
setCur :: State -> Char -> State
setCur s@State {tape = t} c = step $ s {tape = t {cur = ord c}}

-- | validates brainfuck-code by counting brackets and filters out ignored characters
validate :: String -> Maybe String
validate = go 0 ""
  where
    go 0 _ (']':_) = Nothing
    go 0 os [] = Just $ reverse os
    go n os ('[':is) = go (n + 1) ('[' : os) is
    go n os (']':is) = go (n - 1) (']' : os) is
    go n os (i:is)
      | i `elem` "+-<>,." = go n (i : os) is
      | otherwise = go n os is

-- | Returns a clean initial state containing the given program
initialState :: String -> (State -> IO ()) -> State
initialState (p:rog) = State (Tape (repeat 0) 0 (repeat 0)) (Prog [] p rog) []
