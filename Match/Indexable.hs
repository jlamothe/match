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

module Match.Indexable (Indexable (..)) where

import Data.Char
import Data.List
import Match.Types

class Indexable i where
  indexOf :: i -> SearchIndex
  makeIndex :: [i] -> [(SearchIndex, i)]
  makeIndex xs = zip (map indexOf xs) xs

instance Indexable Product where
  indexOf product =
    SearchIndex { titleSignature =
                  signature $ productName product
                , manufacturerSignature =
                  signature $ productManufacturer product
                }

instance Indexable Listing where
  indexOf listing =
    SearchIndex { titleSignature =
                  signature $ listingTitle listing
                , manufacturerSignature =
                  signature $ listingManufacturer listing
                }

signature :: String -> [String]
signature str = dedup $ filterTokens $ foldr buildTokens [[]] sanitizedStr
  where sanitizedStr = map toUpper str

buildTokens :: Char -> [String] -> [String]
buildTokens ch (token : tokens)
  | isAlphaNum ch = (ch : token) : tokens
  | otherwise     = [] : token : tokens

filterTokens :: [String] -> [String]
filterTokens = filter (/= "")

dedup :: [String] -> [String]
dedup xs =
  foldr (\x xs -> case xs of
           []       -> [x]
           (y : _) -> if x /= y
             then x : xs
             else xs) [] $ sort xs

-- jl
