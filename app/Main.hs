{-
 - File              : Main.hs
 - Author            : Vincent Truchseß <redtux@posteo.net>
 - Date              : 25.01.2019
 - Last Modified Date: 28.01.2019
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
            then debug $ initialState (p : rog) debug
            else run $ initialState (p : rog) run
        _ -> return ()

run s@State {runFkt = f} =
  case getCmd s of
    '\0' -> return ()
    '+' -> f $ increment s
    '-' -> f $ decrement s
    '>' -> f $ goRight s
    '<' -> f $ goLeft s
    '[' -> f $ loopBegin s
    ']' -> f $ loopEnd s
    '.' -> do
      putChar $ getCur s
      f $ step s
    ',' -> do
      c <- getChar
      f $ setCur s c

debug :: State -> IO ()
debug s = do
  putStr $ show s
  run s
