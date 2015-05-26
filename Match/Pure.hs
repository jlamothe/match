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

module Match.Pure ( decodeProduct
                  , decodeListing
                  , matchData
                  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Match.Types
import Match.Indexable
import qualified Text.JSON as JSON

decodeProduct :: String -> Maybe Product
decodeProduct = decodeWith productFromJSON

decodeListing :: String -> Maybe Listing
decodeListing = decodeWith listingFromJSON

matchData :: [Product] -> [Listing] -> MatchedData
matchData products listings =
  foldl (findMatch indexedProducts) Map.empty indexedListings
  where
    indexedProducts = makeIndex products
    indexedListings = makeIndex listings

decodeWith :: (JSON.JSObject JSON.JSValue -> Maybe a) -> String -> Maybe a
decodeWith f rawInput = case JSON.decode rawInput of
  JSON.Ok (JSON.JSObject obj) ->
    f obj
  _ -> Nothing

productFromJSON :: JSON.JSObject JSON.JSValue -> Maybe Product
productFromJSON obj = do
  name         <- stringFromObj "product_name" obj
  manufacturer <- stringFromObj "manufacturer" obj
  Just Product { productName         = name
               , productManufacturer = manufacturer
               }

listingFromJSON :: JSON.JSObject JSON.JSValue -> Maybe Listing
listingFromJSON obj = do
  title        <- stringFromObj "title" obj
  manufacturer <- stringFromObj "manufacturer" obj
  currency     <- stringFromObj "currency" obj
  price        <- stringFromObj "price" obj
  Just Listing { listingTitle        = title
               , listingManufacturer = manufacturer
               , listingCurrency     = currency
               , listingPrice        = price
               }

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

findMatch
  :: [(SearchIndex, Product)]
  -> MatchedData
  -> (SearchIndex, Listing)
  -> MatchedData
findMatch [] s _ = s
findMatch ((pIndex, product) : xs) s indexedListing@(lIndex, listing)
  | compareIndex pIndex lIndex = addToMatch product listing s
  | otherwise                  = findMatch xs s indexedListing

compareIndex :: SearchIndex -> SearchIndex -> Bool
compareIndex a b = compareSignature titleA titleB &&
  compareSignature manufacturerA manufacturerB
  where
    titleA        = titleSignature a
    titleB        = titleSignature b
    manufacturerA = manufacturerSignature a
    manufacturerB = manufacturerSignature b

compareSignature :: [String] -> [String] -> Bool
compareSignature a b =
  all (`elem` b) a

addToMatch :: Product -> Listing -> MatchedData -> MatchedData
addToMatch product listing s =
  Map.insert key (listing : prevList) s
  where
    key = productName product
    prevList = Map.findWithDefault [] key s

-- jl
