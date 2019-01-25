{-
 - File              : Lib.hs
 - Author            : Vincent Truchseß <redtux@posteo.net>
 - Date              : 25.01.2019
 - Last Modified Date: 25.01.2019
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

-- Move returns a tape with the head moced to the next cell in the given
-- direction
move dir (Tape (l:ls) c (r:rs)) =
  case dir of
    Ri -> Tape (c : l : ls) r rs
    Le -> Tape ls l (c : r : rs)

data Direction
  = Ri
  | Le

-- Those functions return a tape with the current element being
-- incremented or decremented
increment' t@Tape {cur = c} = t {cur = (c + 1) `mod` 256}

decrement' t@Tape {cur = c} = t {cur = (c - 1) `mod` 256}

-- Helper function to put a number into the current cell. Used in the putChar
-- function in the state implementation
put nc t = t {cur = nc}

-- The programm type holds a tuple of two Strings. The left one contains the 
-- commands already executed, while the right one holds the ones still to come.
newtype Programm =
  Prog (String, String)

-- The show implementation prints a section of the programm markingthe current 
-- comand with | symbols.
instance Show Programm where
  show (Prog (prev, c:cs)) =
    reverse (take 10 prev) ++ "|" ++ [c] ++ "|" ++ reverse (take 10 cs)

-- Helper function extracting the current command
getCmd (Prog (prev, c:cs)) = c

-- Helper function returning a programm one step furtherthen before.
next (Prog (prev, c:cs)) = Prog (c : prev, cs)

-- | Modes are used to define the runtime behaviour of the Brainfuck-Machine
-- | and can contain data needed in future applications. Mode holds the
-- | appropriate run-function for the current machine State. This is not used yet.
-- Right now, tehre is no use for the Debug - type. At the moment the debug 
-- mode is invoked by passing a state with mode = Run debug in the IO area.
data Mode
  = Run { func :: Maybe State -> IO () }
  | Debug { func :: Maybe State -> IO ()
          , seen :: String
          , seenStack :: [String] }

-- | The General machine State. 
data State = State
  { tape :: Tape
  , prog :: Programm
  , stack :: [Programm]
  , mode :: Mode
  }

instance Show State where
  show State {tape = t, prog = p} = "\n" ++ show t ++ "\n" ++ show p ++ "\n"

-- This function is used to remove the current head from the prog-field in the
-- state object. It returns Nothing when the program has terminated.
nx s@State {prog = Prog (prev, c:cs)} =
  case cs of
    [] -> Nothing
    _ -> Just s {prog = Prog (c : prev, cs)}

-- | Performs an increment-operation on the current element of the tape
increment :: State -> Maybe State
increment s@State {tape = t} = nx $ s {tape = increment' t}

-- | Performs a decrement operation on the current element of the tape
decrement :: State -> Maybe State
decrement s@State {tape = t} = nx $ s {tape = decrement' t}

-- | Moves the head tn the tape one cell to the right
goRight :: State -> Maybe State
goRight s@State {tape = t} = nx $ s {tape = move Ri t}

-- | Moves the head on the tape one cell to the left
goLeft :: State -> Maybe State
goLeft s@State {tape = t} = nx $ s {tape = move Le t}

-- | This function handles the beginning of loops
loopBegin :: State -> Maybe State
loopBegin s@State {tape = t, prog = p, stack = st} =
  case cur t of
    0 -> nx $ s {prog = go 1 (next p)}
      where go 0 p = p
            go n p =
              case getCmd p of
                '[' -> go (n + 1) (next p)
                ']' ->
                  if n == 1
                    then p
                    else go (n - 1) (next p)
                _ -> go n (next p)
    _ -> nx $ s {stack = p : st}

-- | This function handles the end of loops
loopEnd :: State -> Maybe State
loopEnd s@State {stack = st, tape = t} =
  case cur t of
    0 -> nx $ s {stack = tail st}
    _ -> nx $ s {prog = head st}

-- | get the current element from the tape as char
getCur :: State -> Char
getCur State {tape = t} = chr $ cur t

-- | set the current element of the tape and go to next command
setCur :: State -> Char -> Maybe State
setCur s@State {tape = t} c = nx $ s {tape = put (ord c) t}

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

-- | Retrieves the currend command from a state-object
cmd :: State -> Char
cmd State {prog = p} = getCmd p

-- | Returns a clean initial state containing the given program
initialState :: String -> Mode -> State
initialState prog = State (Tape (repeat 0) 0 (repeat 0)) (Prog ([], prog)) []
