{-
 - File              : Main.hs
 - Author            : Vincent Truchseß <redtux@posteo.net>
 - Date              : 25.01.2019
 - Last Modified Date: 25.01.2019
 - Last Modified By  : Vincent Truchseß <redtux@posteo.net>
 - app/Main.hs
 -
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
module Main where

import Data.Char
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args
    then return ()
    else do
      code <- readFile $ head args
      let vcode = validate code
      case vcode of
        Just (p:rog) ->
          if length args > 1 && args !! 1 == "debug"
            then debug $ Just $ initialState (p : rog) (Run debug)
            else run $ Just $ initialState (p : rog) (Run run)
        _ -> return ()

run :: Maybe State -> IO ()
run Nothing = return ()
run (Just s@State {mode = m}) =
  case cmd s of
    '+' -> fm $ increment s
    '-' -> fm $ decrement s
    '>' -> fm $ goRight s
    '<' -> fm $ goLeft s
    '[' -> fm $ loopBegin s
    ']' -> fm $ loopEnd s
    '.' -> do
      putChar $ getCur s
      fm $ nx s
    ',' -> do
      c <- getChar
      fm $ setCur s c
  where
    fm = func m

debug :: Maybe State -> IO ()
debug Nothing = return ()
debug (Just s) = do
  putStr $ show s
  run $ Just s
