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

module Match.Pure (matchData) where

import qualified Data.Map as Map
import Match.Types
import Match.Indexable

matchData :: [Product] -> [Listing] -> MatchedData
matchData products listings =
  foldl (findMatch indexedProducts) Map.empty indexedListings
  where
    indexedProducts = makeIndex products
    indexedListings = makeIndex listings

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
