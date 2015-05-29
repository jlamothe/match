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

module Match.JSON (decode, decodeFile) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Match.Types
import Text.JSON (JSON)
import qualified Text.JSON as JSON


decodeFile :: JSON j => FilePath -> IO [j]
decodeFile filePath = do
  contents <- readFile filePath
  return $ mapMaybe decode $ lines contents

decode :: JSON j => String -> Maybe j
decode rawInput = toMaybe $ JSON.decode rawInput

toMaybe :: JSON.Result a -> Maybe a
toMaybe (JSON.Ok val) = Just val
toMaybe _             = Nothing

instance JSON Product where
  readJSON (JSON.JSObject obj) = do
    name         <- stringFromObj "product_name" obj
    manufacturer <- stringFromObj "manufacturer" obj
    JSON.Ok Product { productName         = name
                    , productManufacturer = manufacturer
                    }
  readJSON _ = JSON.Error "could not build Product from JSON"

  showJSON = undefined

instance JSON Listing where
  readJSON (JSON.JSObject obj) = do
    title        <- stringFromObj "title" obj
    manufacturer <- stringFromObj "manufacturer" obj
    currency     <- stringFromObj "currency" obj
    price        <- stringFromObj "price" obj
    JSON.Ok Listing { listingTitle        = title
                    , listingManufacturer = manufacturer
                    , listingCurrency     = currency
                    , listingPrice        = price
                    }
  readJSON _ = JSON.Error "could not build Listing from JSON"

  showJSON x = JSON.JSObject $ JSON.toJSObject
    [ ("title",        JSON.JSString $ JSON.toJSString $ listingTitle x       )
    , ("manufacturer", JSON.JSString $ JSON.toJSString $ listingManufacturer x)
    , ("currency",     JSON.JSString $ JSON.toJSString $ listingCurrency x    )
    , ("price",        JSON.JSString $ JSON.toJSString $ listingPrice x       )
    ]

stringFromObj :: String -> JSON.JSObject JSON.JSValue -> JSON.Result String
stringFromObj key obj = do
  val <- valFromObj key obj
  case val of
    JSON.JSString str ->
      JSON.Ok $ JSON.fromJSString str
    _ -> JSON.Error $ "could not find string at at key " ++ key

valFromObj :: String -> JSON.JSObject JSON.JSValue -> JSON.Result JSON.JSValue
valFromObj key obj =
  case Map.lookup key values of
    Just val -> JSON.Ok val
    Nothing  -> JSON.Error $ "could not find value at key " ++ key
  where values = objToMap obj

objToMap :: JSON.JSObject JSON.JSValue -> Map String JSON.JSValue
objToMap = Map.fromList . JSON.fromJSObject

-- jl
