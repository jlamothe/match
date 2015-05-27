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

module Match.Decodable (Decodable (..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Match.Types
import qualified Text.JSON as JSON

class Decodable d where
  decodeFile :: FilePath -> IO [d]
  decodeFile filePath = do
    contents <- readFile filePath
    return $ mapMaybe decode $ lines contents

  decode :: String -> Maybe d
  decode rawInput = case JSON.decode rawInput of
    JSON.Ok val -> decodeFromJSON val
    _           -> Nothing

  decodeFromJSON :: JSON.JSValue -> Maybe d

instance Decodable Product where
  decodeFromJSON (JSON.JSObject obj) = do
    name         <- stringFromObj "product_name" obj
    manufacturer <- stringFromObj "manufacturer" obj
    Just Product { productName         = name
                 , productManufacturer = manufacturer
                 }
  decodeFromJSON _ = Nothing

instance Decodable Listing where
  decodeFromJSON (JSON.JSObject obj) = do
    title        <- stringFromObj "title" obj
    manufacturer <- stringFromObj "manufacturer" obj
    currency     <- stringFromObj "currency" obj
    price        <- stringFromObj "price" obj
    Just Listing { listingTitle        = title
                 , listingManufacturer = manufacturer
                 , listingCurrency     = currency
                 , listingPrice        = price
                 }
  decodeFromJSON _ = Nothing

stringFromObj :: String -> JSON.JSObject JSON.JSValue -> Maybe String
stringFromObj key obj =
  case valFromObj key obj of
    Just (JSON.JSString str) ->
      Just $ JSON.fromJSString str
    _ -> Nothing

valFromObj :: String -> JSON.JSObject JSON.JSValue -> Maybe JSON.JSValue
valFromObj key obj =
  Map.lookup key values
  where values = objToMap obj

objToMap :: JSON.JSObject JSON.JSValue -> Map String JSON.JSValue
objToMap = Map.fromList . JSON.fromJSObject

-- jl
