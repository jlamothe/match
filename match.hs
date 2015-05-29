-- Match
-- Copyright (C) 2015  Jonathan Lamothe
-- <jonathan@jlamothe.net>

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see
-- <http://www.gnu.org/licenses/>.

module Main (main) where

import Data.Maybe (mapMaybe)
import Match.JSON
import Match.Pure
import Match.Types
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  (productFile, listingFile) <- getParams
  products <- decodeFile productFile
  listings <- decodeFile listingFile
  output $ matchData products listings

getParams :: IO (FilePath, FilePath)
getParams = do
  args <- getArgs
  case args of
    []     -> return ("products.txt", "listings.txt")
    [p, l] -> return (p, l)
    _      -> do
      progName <- getProgName
      error $ "Usage: " ++ progName ++
        " [<path_to_products_file> <path_to_listings_file>]"

output :: MatchedData -> IO ()
output = undefined

-- jl
