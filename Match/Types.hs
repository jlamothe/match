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

module Match.Types ( Product (..)
                   , Listing (..)
                   , SearchIndex (..)
                   , MatchedData
                   ) where

import Data.Map (Map)

data Product =
  Product { productName         :: String
          , productManufacturer :: String
          } deriving (Eq, Show)

data Listing =
  Listing { listingTitle        :: String
          , listingManufacturer :: String
          , listingCurrency     :: String
          , listingPrice        :: String
          } deriving (Eq, Show)

data SearchIndex =
  SearchIndex { titleSignature        :: [String]
              , manufacturerSignature :: [String]
              } deriving (Eq, Show)

type MatchedData = Map String [Listing]

-- jl
